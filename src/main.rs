#![feature(io_error_downcast)]

mod error;

use std::fs::File;
use std::io::{self, Read, Write};
use std::os::fd::{FromRawFd, RawFd};

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
    x: u16,
    y: u16,
}

struct Ked {
    stdin: File,
    stdout: File,
    orig_termios: Termios,
    screen_size: TermSize,
    buf: Vec<u8>,
    cur: Pos,
}

mod escape_seq {
    use crate::error::VoidResult;
    use crate::Pos;

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

    impl Pos {
        /// Write into `buf` the escape sequence needed to move cursor to `self`.
        ///
        /// * Assumes `self` is 0-indexed, whereas the terminal cursor needs to be 1-indexed.
        /// * Also transforms `self.y` into `row` and `self.x` into col.
        pub(crate) fn write_move(&self, buf: &mut dyn std::io::Write) -> VoidResult {
            write!(buf, "\x1b[{};{}H", self.y + 1, self.x + 1)?;
            Ok(())
        }
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
            b"\x1b[" => {
                match &buf[2..n] {
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
                }
            }
            _ => buf[0] as _,
        };
        Ok(key)
    }

    fn process_key(&mut self) -> VoidResult {
        let c = self.read_key()?;
        log::trace!(target: "keytrace", "Key {c}");
        match c {
            k if k == ctrl_key(b'q') as _ => return Err(KError::Quit),
            // probably need to accomodate scrolling in the future
            keys::UP => self.cur.y = self.cur.y.saturating_sub(1),
            keys::DOWN => self.cur.y = (self.cur.y + 1).min(self.screen_size.row - 1),
            keys::LEFT => self.cur.x = self.cur.x.saturating_sub(1),
            keys::RIGHT => self.cur.x = (self.cur.x + 1).min(self.screen_size.col - 1),
            keys::PGUP => self.cur.y = self.cur.y.saturating_sub(self.screen_size.row - 1),
            keys::PGDOWN => self.cur.y = self.screen_size.row - 1,
            keys::HOME => self.cur.x = 0,
            keys::END => self.cur.x = self.screen_size.col - 1,
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

    fn clear_screen(&mut self) -> VoidResult {
        self.buf.clear();
        esc_write!(self.buf, CLEAR)?;
        esc_write!(self.buf, RESET_CURSOR)?;
        self.flush_buf()?;
        Ok(())
    }

    fn refresh_screen(&mut self) -> VoidResult {
        self.buf.clear();
        esc_write!(self.buf, HIDE_CURSOR)?;
        esc_write!(self.buf, RESET_CURSOR)?;
        self.draw_rows()?;
        self.write_move_cur()?;
        esc_write!(self.buf, UNHIDE_CURSOR)?;
        self.flush_buf()?;
        Ok(())
    }

    /// Write into `self.buf` the escape sequence needed to move cursor to `self.cur`.
    fn write_move_cur(&mut self) -> VoidResult {
        self.cur.write_move(&mut self.buf)
    }

    fn flush_buf(&mut self) -> VoidResult {
        self.stdout.write_all(self.buf.as_slice())?;
        Ok(())
    }

    fn draw_rows(&mut self) -> VoidResult {
        for y in 0..self.screen_size.row {
            if y == self.screen_size.row / 3 {
                write!(
                    self.buf,
                    "{:^width$}",
                    concat!("Welcome to ked -- ", env!("CARGO_PKG_VERSION")),
                    width = self.screen_size.col as usize
                )?;
            } else {
                write!(self.buf, "~")?;
            }
            esc_write!(self.buf, CLEAR_TRAIL_LINE)?;
            if y < self.screen_size.row - 1 {
                write!(self.buf, "\r\n")?;
            }
        }
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
    enable_raw_mode().expect("failed to enable raw");
    loop {
        ked.refresh_screen()?;
        if let Err(e) = ked.process_key() {
            ked.clear_screen()?;
            if e.is_quit() {
                // just a simple quit
            } else {
                // need to reset the termios before printing errors.
                drop(ked);
                log::error!("Error! {e}");
            }
            break;
        }
    }
    Ok(())
}
