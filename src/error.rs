use std::error::Error;
use std::fmt::Display;
use std::io;

#[derive(Debug)]
pub(crate) enum KError {
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

impl KError {
    pub(crate) fn is_quit(&self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(&Self::Quit)
    }
}

pub(crate) type KResult<T> = Result<T, KError>;
pub(crate) type VoidResult = Result<(), KError>;
