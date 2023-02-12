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
    x: libc::c_ushort,
    y: libc::c_ushort,
}

struct Ked {
    stdin: File,
    stdout: File,
    orig_termios: Termios,
    screen_size: TermSize,
    buf: Vec<u8>,
}

mod escape_seq {
    pub(crate) const CLEAR: &[u8] = b"2J";
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
    fn read_key(&mut self) -> KResult<u8> {
        let mut c: u8 = 0;
        let buf = std::slice::from_mut(&mut c);
        let _ = self.stdin.read(buf)?;
        Ok(c)
    }
    fn process_key(&mut self) -> VoidResult {
        let c = self.read_key()?;
        log::trace!(target: "keytrace", "Key {c}");
        match c {
            k if k == ctrl_key(b'q') => return Err(KError::Quit),
            _ => {
                let ch: char = c.into();
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
        esc_write!(self.buf, CLEAR)?;
        esc_write!(self.buf, RESET_CURSOR)?;
        Ok(())
    }

    fn refresh_screen(&mut self) -> VoidResult {
        self.buf.clear();
        self.clear_screen()?;
        self.draw_rows()?;
        esc_write!(self.buf, RESET_CURSOR)?;
        self.flush_buf()?;
        Ok(())
    }

    fn flush_buf(&mut self) -> VoidResult {
        self.stdout.write_all(self.buf.as_slice())?;
        Ok(())
    }

    fn draw_rows(&mut self) -> VoidResult {
        for i in 0..self.screen_size.row {
            write!(self.buf, "~{i}")?;
            if i < self.screen_size.row - 1 {
                self.buf.write_all(b"\r\n")?;
            }
        }
        Ok(())
    }
}

impl Drop for Ked {
    fn drop(&mut self) {
        self.clear_screen()
            .and_then(|_| self.flush_buf())
            .unwrap_or_else(|e| {
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
            ked.clear_screen().and_then(|_| ked.flush_buf())?;
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
