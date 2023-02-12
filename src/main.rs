use std::io::{self, Read};
use std::os::fd::RawFd;

use log::error;
use termios::{self, Termios};

const STDIN_FD: RawFd = 0;
fn enable_raw_mode() -> io::Result<()> {
    let mut t = termios::Termios::from_fd(STDIN_FD)?;
    termios::tcgetattr(STDIN_FD, &mut t)?;
    t.c_lflag &= !termios::ECHO;
    termios::tcsetattr(STDIN_FD, termios::TCSAFLUSH, &t)?;
    Ok(())
}

struct Cleanup {
    orig_termios: Termios,
}

impl Cleanup {
    fn new() -> io::Result<Self> {
        Ok(Cleanup {
            orig_termios: Termios::from_fd(STDIN_FD)?,
        })
    }
    fn disable_raw_mode(&self) {
        termios::tcsetattr(STDIN_FD, termios::TCSAFLUSH, &self.orig_termios).unwrap_or_else(|e| {
            error!("Failed to disable raw mode: {e}");
        });
    }
}

impl Drop for Cleanup {
    fn drop(&mut self) {
        self.disable_raw_mode();
    }
}

fn main() -> io::Result<()> {
    let mut stdin = std::io::stdin();
    let _ = Cleanup::new();
    enable_raw_mode().expect("failed to enable raw");
    // let lock = stdin.lock();
    let mut buf = [0; 1];
    while stdin.read(&mut buf)? == 1 && buf[0] != b'q' {
        // read
    }
    Ok(())
}
