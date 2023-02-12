#![feature(io_error_downcast)]

mod error;

use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::os::fd::{FromRawFd, RawFd};
use std::path::Path;

use log::error;
use termios::{self, Termios};

use crate::error::{KError, KResult, VoidResult};

const STDIN_FD: RawFd = libc::STDIN_FILENO;
const STDOUT_FD: RawFd = libc::STDOUT_FILENO;

fn enable_raw_mode() -> VoidResult {
    use termios::{
        BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, INPCK, ISIG, ISTRIP, IXON, OPOST, VMIN, VTIME,
    };
    let mut t = termios::Termios::from_fd(STDIN_FD)?;
    termios::tcgetattr(STDIN_FD, &mut t)?;
    t.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    t.c_oflag &= !(OPOST);
    t.c_cflag |= CS8;
    t.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    t.c_cc[VMIN] = 0;
    t.c_cc[VTIME] = 1;
    termios::tcsetattr(STDIN_FD, termios::TCSAFLUSH, &t)?;
    Ok(())
}

const fn ctrl_key(k: u8) -> u8 {
    k & 0b0001_1111
}

mod escape_seq {
    pub(crate) const CLEAR: &[u8] = b"2J";
    pub(crate) const CLEAR_TRAIL_LINE: &[u8] = b"K";
    pub(crate) const HIDE_CURSOR: &[u8] = b"?25l";
    pub(crate) const UNHIDE_CURSOR: &[u8] = b"?25h";
    pub(crate) const RESET_CURSOR: &[u8] = b"1;1H";

    #[allow(unused_macros)]
    macro_rules! move_cursor {
        ($row: literal, $col: literal) => {{
            const ROW: u16 = $row;
            const COL: u16 = $col;
            const S_ROW: &str = const_str::to_str!(ROW);
            const S_COL: &str = const_str::to_str!(COL);
            const B_ROW: [u8; S_ROW.len()] = const_str::to_byte_array!(S_ROW);
            const B_COL: [u8; S_COL.len()] = const_str::to_byte_array!(S_COL);
            const_str::concat_bytes!(B_ROW, b";", B_COL, b"H")
        }};
    }
}

mod keys {
    pub(crate) const UP: u32 = 1000;
    pub(crate) const DOWN: u32 = 1001;
    pub(crate) const LEFT: u32 = 1002;
    pub(crate) const RIGHT: u32 = 1003;
    pub(crate) const PGUP: u32 = 1004;
    pub(crate) const PGDOWN: u32 = 1005;
    pub(crate) const HOME: u32 = 1006;
    pub(crate) const END: u32 = 1007;
}

macro_rules! esc_write {
    ($file: expr, $val: ident) => {
        $file.write_all(const_str::concat_bytes!(b"\x1b[", escape_seq::$val))
    };
    ($file: expr, $val: expr) => {
        $file.write_all(const_str::concat_bytes!(b"\x1b[", $val))
    };
}

#[derive(Debug)]
#[repr(C)]
struct TermSize {
    row: libc::c_ushort,
    col: libc::c_ushort,
    _x: libc::c_ushort,
    _y: libc::c_ushort,
}

#[derive(Debug, Default)]
struct Pos {
    x: usize,
    y: usize,
}

struct Ked {
    stdin: File,
    stdout: File,
    orig_termios: Termios,
    screen_size: TermSize,
    buf: Vec<u8>,
    cur: Pos,
    rows: Vec<String>,
    render_rows: Vec<String>,
    render_pos_x: usize,
    rowoff: usize,
    coloff: usize,
}

const TAB_STOP: usize = 4;

impl Ked {
    fn new() -> KResult<Self> {
        Ok(Ked {
            stdin: unsafe { File::from_raw_fd(STDIN_FD) },
            stdout: unsafe { File::from_raw_fd(STDOUT_FD) },
            orig_termios: Termios::from_fd(STDIN_FD)?,
            screen_size: Self::get_window_size()?,
            // assume that we'll need to write at least these many bytes
            buf: Vec::with_capacity(48),
            cur: Default::default(),
            rows: Vec::new(),
            rowoff: 0,
            coloff: 0,
            render_rows: Vec::new(),
            render_pos_x: 0,
        })
    }

    fn get_window_size() -> KResult<TermSize> {
        unsafe {
            let mut size: TermSize = std::mem::zeroed();
            let res = libc::ioctl(STDOUT_FD, libc::TIOCGWINSZ, &mut size as *mut _);
            if res == -1 {
                Err(io::Error::last_os_error().into())
            } else {
                log::trace!("size: {size:?}");
                Ok(size)
            }
        }
    }

    fn disable_raw_mode(&self) {
        termios::tcsetattr(STDIN_FD, termios::TCSAFLUSH, &self.orig_termios).unwrap_or_else(|e| {
            error!("Failed to disable raw mode: {e}");
        });
    }
    fn read_key(&mut self) -> KResult<u32> {
        let mut buf = [0; 4];
        let mut n: usize;
        // block till we read *something*
        loop {
            n = self.stdin.read(&mut buf)?;
            if n != 0 {
                break;
            }
        }
        let key: u32 = match &buf[0..2] {
            b"\x1b[" => match &buf[2..n] {
                b"A" => keys::UP,
                b"B" => keys::DOWN,
                b"C" => keys::RIGHT,
                b"D" => keys::LEFT,
                b"H" => keys::HOME,
                b"F" => keys::END,
                b"1~" | b"7~" => keys::HOME,
                b"4~" | b"8~" => keys::END,
                b"5~" => keys::PGUP,
                b"6~" => keys::PGDOWN,
                _ => {
                    log::warn!("Weird data on stdin: {buf:?}");
                    b'\x1b' as _
                }
            },
            _ => buf[0] as _,
        };
        Ok(key)
    }

    fn process_key(&mut self) -> VoidResult {
        let c = self.read_key()?;
        log::trace!(target: "keytrace", "Key {c}");
        match c {
            k if k == ctrl_key(b'q') as _ => return Err(KError::Quit),
            keys::UP
            | keys::DOWN
            | keys::LEFT
            | keys::RIGHT
            | keys::PGUP
            | keys::PGDOWN
            | keys::HOME
            | keys::END => self.move_cursor(c),
            _ => {
                let ch: char = char::from_u32(c).expect("invalid char");
                if ch.is_ascii_control() {
                    write!(self.buf, "{c}\r")?;
                } else {
                    write!(self.buf, "{c} ('{ch}')\r")?;
                }
            }
        }
        Ok(())
    }

    fn move_cursor(&mut self, key: u32) {
        let row = self.rows.get(self.cur.y);
        match key {
            keys::UP => self.cur.y = self.cur.y.saturating_sub(1),
            keys::DOWN => self.cur.y = (self.cur.y + 1).min(self.rows.len()),
            keys::LEFT => {
                if self.cur.x == 0 {
                    self.cur.y = self.cur.y.saturating_sub(1);
                    self.cur.x = self.rows.get(self.cur.y).map_or(0, |row| row.len());
                } else {
                    self.cur.x -= 1;
                }
            }
            keys::RIGHT => {
                if self.cur.x == row.map_or(self.cur.x, |row| row.len()) {
                    self.cur.y = (self.cur.y + 1).min(self.rows.len());
                    self.cur.x = 0;
                } else {
                    self.cur.x += 1;
                }
            }
            keys::PGUP => self.cur.y = self.cur.y.saturating_sub(self.screen_size.row as usize - 1),
            keys::PGDOWN => {
                self.cur.y = (self.cur.y + self.screen_size.row as usize - 1).min(self.rows.len())
            }
            keys::HOME => self.cur.x = 0,
            keys::END => self.cur.x = row.map_or(0, |row| row.len()),
            _ => panic!("Unknown movement key"),
        }
        let row = self.rows.get(self.cur.y);
        self.cur.x = self.cur.x.min(row.map_or(0, |row| row.len()));
    }

    fn clear_screen(&mut self) -> VoidResult {
        self.buf.clear();
        esc_write!(self.buf, CLEAR)?;
        esc_write!(self.buf, RESET_CURSOR)?;
        self.flush_buf()?;
        Ok(())
    }

    fn scroll_screen(&mut self) {
        // convert from "document" coordinate to "rendered" coordinate
        self.render_pos_x = self.rows.get(self.cur.y).map_or(0, |row| {
            row.chars().take(self.cur.x).fold(0, |acc, c| {
                acc + if c == '\t' {
                    TAB_STOP - (acc % TAB_STOP)
                } else {
                    1
                }
            })
        });

        self.rowoff = self.rowoff.min(self.cur.y);
        if self.cur.y >= self.rowoff + self.screen_size.row as usize {
            self.rowoff = self.cur.y.saturating_sub(self.screen_size.row as usize) + 1;
        }
        self.coloff = self.coloff.min(self.render_pos_x);
        if self.render_pos_x >= self.coloff + self.screen_size.col as usize {
            self.coloff = self
                .render_pos_x
                .saturating_sub(self.screen_size.col as usize)
                + 1;
        }
    }

    fn refresh_screen(&mut self) -> VoidResult {
        self.buf.clear();
        self.scroll_screen();
        esc_write!(self.buf, HIDE_CURSOR)?;
        esc_write!(self.buf, RESET_CURSOR)?;
        self.draw_rows()?;
        self.write_move_cur()?;
        esc_write!(self.buf, UNHIDE_CURSOR)?;
        self.flush_buf()?;
        Ok(())
    }

    /// Write into `self.buf` the escape sequence needed to move cursor to `self.cur`.
    ///
    /// * Assumes `self.cur` is 0-indexed, whereas the terminal cursor needs to be 1-indexed.
    /// * Also transforms `self.cur.y` into `row` and `self.render_pos_x` into `col`.
    fn write_move_cur(&mut self) -> VoidResult {
        write!(
            self.buf,
            "\x1b[{};{}H",
            self.cur.y - self.rowoff + 1,
            self.render_pos_x - self.coloff + 1
        )?;
        Ok(())
    }

    fn flush_buf(&mut self) -> VoidResult {
        self.stdout.write_all(self.buf.as_slice())?;
        Ok(())
    }

    fn draw_rows(&mut self) -> VoidResult {
        for y in 0..self.screen_size.row as usize {
            let filerow = y + self.rowoff;
            if filerow >= self.rows.len() {
                if self.rows.is_empty() && y == self.screen_size.row as usize / 3 {
                    write!(
                        self.buf,
                        "{:^width$}",
                        concat!("Welcome to ked -- ", env!("CARGO_PKG_VERSION")),
                        width = self.screen_size.col as usize
                    )?;
                } else {
                    write!(self.buf, "~")?;
                }
            } else {
                let row = &self.render_rows[filerow];
                // only show the line if it is visible region
                if self.coloff <= row.len() {
                    // need to clip manually. using std::fmt's width option causes line wraps.
                    let clip_end = row.len().min(self.coloff + self.screen_size.col as usize);
                    let clipped = &row[self.coloff..clip_end];
                    write!(self.buf, "{clipped}")?;
                }
            }
            esc_write!(self.buf, CLEAR_TRAIL_LINE)?;
            if y < self.screen_size.row as usize - 1 {
                write!(self.buf, "\r\n")?;
            }
        }
        Ok(())
    }

    fn open(&mut self, path: impl AsRef<Path>) -> VoidResult {
        let f = std::fs::OpenOptions::new()
            .read(true)
            .write(false)
            .open(path.as_ref())?;
        let reader = BufReader::new(f);
        self.rows = reader.lines().collect::<Result<Vec<_>, _>>()?;
        self.render_rows = self
            .rows
            .iter()
            .map(|line| {
                let mut out = String::with_capacity(
                    line.chars()
                        .map(|c| if c == '\t' { TAB_STOP } else { 1 })
                        .sum(),
                );
                line.chars().fold(0, |acc, c| {
                    acc + match c {
                        '\t' => {
                            let extra_spaces = TAB_STOP - (acc % TAB_STOP);
                            out.extend(std::iter::repeat(' ').take(extra_spaces));
                            extra_spaces
                        }
                        _ => {
                            out.push(c);
                            1
                        }
                    }
                });
                out
            })
            .collect();
        log::trace!(
            "Opened file: {} with {} lines.",
            path.as_ref().display(),
            self.rows.len(),
        );
        Ok(())
    }
}

impl Drop for Ked {
    fn drop(&mut self) {
        self.clear_screen().unwrap_or_else(|e| {
            error!("Failed to clear screen on exit: {e}");
        });
        self.disable_raw_mode();
    }
}

fn main() -> VoidResult {
    let log_file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open("ked.log")?;
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .filter_module("keytrace", log::LevelFilter::Info)
        .parse_default_env()
        .target(env_logger::Target::Pipe(Box::new(log_file)))
        .init();

    let mut ked = Ked::new()?;
    if let Some(path) = std::env::args().nth(1) {
        ked.open(path)?;
    }
    enable_raw_mode().expect("failed to enable raw");
    loop {
        ked.refresh_screen()?;
        if let Err(e) = ked.process_key() {
            ked.clear_screen()?;
            if e.is_quit() {
                // just a simple quit
            } else {
                // need to reset the termios before printing errors.
                log::error!("Error! {e}");
            }
            break;
        }
    }
    drop(ked);
    Ok(())
}
