use std::io::{self, Read, StdinLock};
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

const fn ctrl_key(k: u8) -> u8 {
    k & 0b0001_1111
}

struct Ked {
    stdin: StdinLock<'static>,
    orig_termios: Termios,
}

impl Ked {
    fn new() -> io::Result<Self> {
        Ok(Ked {
            stdin: std::io::stdin().lock(),
            orig_termios: Termios::from_fd(STDIN_FD)?,
        })
    }
    fn disable_raw_mode(&self) {
        termios::tcsetattr(STDIN_FD, termios::TCSAFLUSH, &self.orig_termios).unwrap_or_else(|e| {
            error!("Failed to disable raw mode: {e}");
        });
    }
    fn read_key(&mut self) -> io::Result<u8> {
        let mut c: u8 = 0;
        let buf = std::slice::from_mut(&mut c);
        self.stdin.read(buf)?;
        Ok(c)
    }
    fn process_key(&mut self) -> io::Result<()> {
        let c = self.read_key()?;
        log::trace!("Key {c}");
        match c {
            k if k == ctrl_key(b'q') => return Err(io::Error::new(io::ErrorKind::Other, "quit")),
            _ => {
                let ch: char = c.into();
                if ch.is_ascii_control() {
                    println!("{c}\r");
                } else {
                    println!("{c} ('{ch}')\r");
                }
            }
        }
        Ok(())
    }
}

impl Drop for Ked {
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
        .filter_level(log::LevelFilter::Debug)
        .parse_default_env()
        .target(env_logger::Target::Pipe(Box::new(log_file)))
        .init();
    // let mut stdin = ;
    let mut ked = Ked::new()?;
    enable_raw_mode().expect("failed to enable raw");
    loop {
        if let Err(e) = ked.process_key() {
            log::error!("Error! {e}");
            break;
        }
    }
    drop(ked);
    Ok(())
}
