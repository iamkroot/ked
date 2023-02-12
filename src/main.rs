use std::io::{self, Read};
use std::os::fd::RawFd;

use log::error;
use termios::{self, Termios};

const STDIN_FD: RawFd = 0;
fn enable_raw_mode() -> io::Result<()> {
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
    let log_file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open("ked.log")?;
    env_logger::builder()
        .parse_default_env()
        .filter_level(log::LevelFilter::Debug)
        .target(env_logger::Target::Pipe(Box::new(log_file)))
        .init();
    let mut stdin = std::io::stdin().lock();
    let cleanup = Cleanup::new()?;
    enable_raw_mode().expect("failed to enable raw");
    let mut buf = [0; 1];
    loop {
        buf[0] = 0;
        stdin.read(&mut buf)?;
        let c = char::from(buf[0]);
        if c.is_ascii_control() {
            println!("{}\r", buf[0]);
        } else {
            println!("{} ('{c}')\r", buf[0]);
        }
        if c == 'q' {
            break;
        }
    }
    drop(cleanup);
    Ok(())
}
