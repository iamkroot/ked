#![feature(io_error_downcast)]

use std::error::Error;
use std::fmt::Display;
use std::io::{self, Read, StdinLock};
use std::os::fd::RawFd;

use log::error;
use termios::{self, Termios};

const STDIN_FD: RawFd = 0;

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
enum KError {
    Io(io::Error),
    Quit,
    #[allow(dead_code)]
    SimpleMessage(&'static str),
}

impl Display for KError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KError::Io(e) => write!(f, "io: {e}"),
            KError::Quit => write!(f, "quit!"),
            KError::SimpleMessage(msg) => write!(f, "error: {msg}"),
        }
    }
}

impl Error for KError {}

impl From<io::Error> for KError {
    fn from(value: io::Error) -> Self {
        value
            .downcast::<Self>()
            .map(|b| *b)
            .unwrap_or_else(Self::Io)
    }
}

type KResult<T> = Result<T, KError>;
type VoidResult = Result<(), KError>;

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
    fn read_key(&mut self) -> KResult<u8> {
        let mut c: u8 = 0;
        let buf = std::slice::from_mut(&mut c);
        self.stdin.read(buf)?;
        Ok(c)
    }
    fn process_key(&mut self) -> VoidResult {
        let c = self.read_key()?;
        log::trace!("Key {c}");
        match c {
            k if k == ctrl_key(b'q') => return Err(KError::Quit),
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
    fn refresh_screen(&mut self) -> VoidResult {
        todo!()
    }
}

impl Drop for Ked {
    fn drop(&mut self) {
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
        .parse_default_env()
        .target(env_logger::Target::Pipe(Box::new(log_file)))
        .init();

    let mut ked = Ked::new()?;
    enable_raw_mode().expect("failed to enable raw");
    loop {
        if let Err(e) = ked.process_key() {
            if core::mem::discriminant(&e) == core::mem::discriminant(&KError::Quit) {
                // just a simple quit
                break;
            }
            log::error!("Error! {e}");
            break;
        }
    }
    drop(ked);
    Ok(())
}
