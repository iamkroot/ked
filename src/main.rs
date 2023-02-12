#![feature(io_error_downcast)]

mod error;

use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::iter;
use std::os::fd::{FromRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

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
    pub(crate) const INVERT_COLOR: &[u8] = b"7m";
    pub(crate) const NORMAL_COLOR: &[u8] = b"m";
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

/// Trims the value with "..." if it exceeds `max_width`
macro_rules! write_trim {
    ($file: expr, $value: expr, $max_width: tt) => {
        write!(
            $file,
            "{}{}",
            if $value.len() > $max_width {
                &$value[..$max_width - 3]
            } else {
                $value
            },
            if $value.len() > $max_width { "..." } else { "" }
        )
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
    filepath: Option<PathBuf>,
    status_msg: (String, Instant),
}

const TAB_STOP: usize = 4;

impl Ked {
    fn new() -> KResult<Self> {
        let screen_size = {
            let mut s = Self::get_window_size()?;
            // leave one row for the status bar, and another for message
            s.row -= 2;
            s
        };
        Ok(Ked {
            stdin: unsafe { File::from_raw_fd(STDIN_FD) },
            stdout: unsafe { File::from_raw_fd(STDOUT_FD) },
            orig_termios: Termios::from_fd(STDIN_FD)?,
            screen_size,
            // assume that we'll need to write at least these many bytes
            buf: Vec::with_capacity(48),
            cur: Default::default(),
            rows: Vec::new(),
            rowoff: 0,
            coloff: 0,
            render_rows: Vec::new(),
            render_pos_x: 0,
            filepath: None,
            status_msg: (Default::default(), Instant::now()),
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
        self.draw_status_bar()?;
        write!(self.buf, "\r\n")?;
        self.draw_status_message()?;
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
            write!(self.buf, "\r\n")?;
        }
        Ok(())
    }

    fn draw_status_bar(&mut self) -> VoidResult {
        esc_write!(self.buf, INVERT_COLOR)?;
        // write file name and number of lines
        let name = self
            .filepath
            .as_ref()
            .and_then(|p| p.file_name())
            .and_then(|s| s.to_str())
            .unwrap_or("[No name]");
        const MAX_NAME_WIDTH: usize = 20;

        let start_len = self.buf.len();
        write_trim!(self.buf, name, MAX_NAME_WIDTH)?;
        if self.filepath.is_some() {
            write!(self.buf, " - {} lines", self.rows.len())?;
        }
        let end_len = self.buf.len();

        // format the rstatus into a buf first
        let rstatus: &mut [u8] = &mut [0u8; 40];
        let mut cur = std::io::Cursor::new(rstatus);
        write!(cur, "{}/{}", self.cur.y + 1, self.rows.len())?;
        let num_rbytes = cur.position() as usize;
        let rstatus = cur.into_inner();

        let sep_space = self.screen_size.col as usize - (end_len - start_len) - num_rbytes;
        self.buf.extend(iter::repeat(b' ').take(sep_space));
        self.buf.extend(rstatus.iter());
        esc_write!(self.buf, NORMAL_COLOR)?;
        Ok(())
    }

    fn draw_status_message(&mut self) -> VoidResult {
        let n = self.screen_size.col as usize;
        if !self.status_msg.0.is_empty() && self.status_msg.1.elapsed() < Duration::from_secs(5) {
            write_trim!(self.buf, &self.status_msg.0, n)?;
        } else {
            self.buf.extend(iter::repeat(b' ').take(n));
        }
        Ok(())
    }

    fn set_status_message(&mut self, fmt_args: std::fmt::Arguments) {
        self.status_msg.0 = fmt_args.to_string();
        self.status_msg.1 = Instant::now();
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
                            out.extend(iter::repeat(' ').take(extra_spaces));
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
        self.filepath = Some(path.as_ref().to_path_buf());
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
    ked.set_status_message(format_args!("HELP: Press Ctrl+q to quit"));
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
