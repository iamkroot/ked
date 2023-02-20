#![feature(io_error_downcast)]
#![feature(write_all_vectored)]
#![feature(get_many_mut)]
#![feature(once_cell)]

mod error;
mod utils;

use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::iter;
use std::os::fd::{FromRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use log::error;
use termios::{self, Termios};

use crate::error::{KError, KResult, VoidResult};
use crate::utils::MinMax;

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
    use crate::Pos;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum Dir {
        Up,
        Down,
        Left,
        Right,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum Key {
        Arrow(Dir),
        Page(Dir),
        Home,
        End,
        Enter,
        Backspace,
        Delete,
        Esc,
        Chr(char),
    }

    // We could use bitflags for this, but meh
    #[allow(unused)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum Modifier {
        None,
        Ctrl,
        Shift,
        Alt,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) struct KeyEvent {
        pub(crate) key: Key,
        pub(crate) modifiers: Modifier,
    }
    impl KeyEvent {
        pub(crate) fn new(key: Key) -> KeyEvent {
            KeyEvent {
                key,
                modifiers: Modifier::None,
            }
        }
    }

    #[allow(unused)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum MouseButton {
        Left,
        Right,
        Middle,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum MouseEventKind {
        Press(MouseButton),
        Release(MouseButton),
        Drag(MouseButton),
        Moved,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) struct MouseEvent {
        pub(crate) kind: MouseEventKind,
        pub(crate) pos: Pos,
        pub(crate) modifiers: Modifier,
    }

    impl MouseEvent {
        pub(crate) fn new(kind: MouseEventKind, pos: Pos) -> MouseEvent {
            MouseEvent {
                kind,
                pos,
                modifiers: Modifier::None,
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum Event {
        Key(KeyEvent),
        Mouse(MouseEvent),
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Pos {
    y: usize,
    x: usize,
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
    dirty_count: u32,
    quit_count: u32,
    show_line_nums: bool,
    /// Range of highlighting, if any
    highlight: Option<(Pos, Pos)>,
}

const TAB_STOP: usize = 4;
const QUIT_TIMES: u32 = 3;

type PromptCB = fn(&mut Ked, &str, keys::Event) -> KResult<()>;

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
            dirty_count: 0,
            quit_count: QUIT_TIMES,
            show_line_nums: false,
            highlight: None,
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

    fn enable_mouse_events(&mut self) -> VoidResult {
        esc_write!(self.stdout, "?1002h")?;
        Ok(())
    }

    fn disable_mouse_events(&mut self) -> VoidResult {
        esc_write!(self.stdout, "?1002l")?;
        Ok(())
    }

    fn read_key(&mut self) -> KResult<keys::Event> {
        use keys::{Dir::*, Event, Key::*, KeyEvent, MouseButton, MouseEvent, MouseEventKind};
        let mut buf = [0; 6];
        let mut n: usize;
        // block till we read *something*
        loop {
            n = self.stdin.read(&mut buf)?;
            if n != 0 {
                break;
            }
        }
        log::trace!(target: "read_key", "readkey {buf:?}");
        let key: keys::Event = match &buf[0..2] {
            b"\x1b[" => match &buf[2..n] {
                b"A" => Event::Key(KeyEvent::new(Arrow(Up))),
                b"B" => Event::Key(KeyEvent::new(Arrow(Down))),
                b"C" => Event::Key(KeyEvent::new(Arrow(Right))),
                b"D" => Event::Key(KeyEvent::new(Arrow(Left))),
                b"H" => Event::Key(KeyEvent::new(Home)),
                b"F" => Event::Key(KeyEvent::new(End)),
                b"1~" | b"7~" => Event::Key(KeyEvent::new(Home)),
                b"4~" | b"8~" => Event::Key(KeyEvent::new(End)),
                b"3~" => Event::Key(KeyEvent::new(Delete)),
                b"5~" => Event::Key(KeyEvent::new(Page(Up))),
                b"6~" => Event::Key(KeyEvent::new(Page(Down))),
                _ if n == 6 => match buf[2] {
                    b'M' => {
                        let row = usize::from(buf[4]).saturating_sub(32) - 1;
                        let col = usize::from(buf[5]).saturating_sub(32) - 1;
                        let pos = Pos { x: row, y: col };
                        let button = (buf[3] & 0b0000_0011) | ((buf[3] & 0b1100_0000) >> 4);
                        let dragging = (buf[3] & 0b0110_0000) > 0;
                        log::trace!(target: "read_key", "Mouse {button} is dragging: {dragging} {} {}", buf[3], buf[3] & 32);
                        let kind = match (button, dragging) {
                            (0, false) => MouseEventKind::Press(MouseButton::Left),
                            (1, false) => MouseEventKind::Press(MouseButton::Right),
                            (2, false) => MouseEventKind::Press(MouseButton::Middle),
                            (0, true) => MouseEventKind::Drag(MouseButton::Left),
                            (1, true) => MouseEventKind::Drag(MouseButton::Right),
                            (2, true) => MouseEventKind::Drag(MouseButton::Middle),
                            (3, _) => MouseEventKind::Release(MouseButton::Left),
                            (4 | 5, true) => MouseEventKind::Moved,
                            _ => {
                                log::warn!("Unknown mouse event: {buf:?}");
                                MouseEventKind::Release(MouseButton::Left)
                            }
                        };
                        log::info!(target: "read_key", "Mouse event {kind:?} at coords: {pos:?}");
                        Event::Mouse(MouseEvent::new(kind, pos))
                    }
                    _ => Event::Key(KeyEvent::new(Esc)),
                },
                _ => {
                    log::warn!(target: "read_key", "Weird data on stdin: {buf:?}");
                    Event::Key(KeyEvent::new(Esc))
                }
            },
            _ => match buf[0] {
                b'\x1b' => Event::Key(KeyEvent::new(Esc)),
                b'\r' => Event::Key(KeyEvent::new(Enter)),
                c @ b'\x01'..=b'\x1a' => Event::Key(KeyEvent {
                    key: Chr((c - 0x1 + b'a') as char),
                    modifiers: keys::Modifier::Ctrl,
                }),
                c @ b'\x1c'..=b'\x1f' => Event::Key(KeyEvent {
                    key: Chr((c - 0xc1 + 4) as char),
                    modifiers: keys::Modifier::Ctrl,
                }),
                b'\0' => Event::Key(KeyEvent {
                    key: Chr(' '),
                    modifiers: keys::Modifier::Ctrl,
                }),
                b'\x7f' => Event::Key(KeyEvent::new(Backspace)),
                _ => Event::Key(KeyEvent::new(Chr(
                    char::from_u32(buf[0] as _).expect("Invalid char")
                ))),
            },
        };
        Ok(key)
    }

    fn process_key(&mut self) -> VoidResult {
        use keys::{Event, Key::*, KeyEvent, Modifier::*, MouseEvent, MouseEventKind};

        let c = self.read_key()?;
        log::trace!(target: "keytrace", "Key {c:?}");
        match c {
            Event::Key(KeyEvent { key: Enter, .. }) => {
                self.insert_newline();
            }
            Event::Key(KeyEvent {
                key: Chr('q'),
                modifiers: Ctrl,
            }) if (self.dirty_count == 0 || self.quit_count == 0) => return Err(KError::Quit),
            Event::Key(KeyEvent {
                key: Chr('q'),
                modifiers: Ctrl,
            }) => {
                let quit_count = self.quit_count;
                self.set_status_message(format_args!(
                    concat!(
                        "Warning! File has unsaved changes. ",
                        "Press Ctrl+Q {} more times to quit"
                    ),
                    quit_count
                ));
                self.quit_count -= 1;
                return Ok(());
            }
            Event::Key(KeyEvent {
                key: Chr('s'),
                modifiers: Ctrl,
            }) => self.save()?,
            Event::Key(KeyEvent {
                key: Chr('f'),
                modifiers: Ctrl,
            }) => self.find()?,
            Event::Key(KeyEvent {
                key: Chr('l'),
                modifiers: Ctrl,
            }) => {
                self.show_line_nums = !self.show_line_nums;
            }

            Event::Key(KeyEvent {
                key: Chr('h'),
                modifiers: Ctrl,
            })
            | Event::Key(KeyEvent { key: Backspace, .. }) => {
                if self.cur.y == self.rows.len() {
                    // we are at the dummy row, move back.
                    self.move_cursor(Arrow(keys::Dir::Left));
                } else {
                    self.del_char()
                }
            }
            Event::Key(KeyEvent { key: Delete, .. }) => {
                self.move_cursor(Arrow(keys::Dir::Right));
                self.del_char();
            }
            Event::Key(KeyEvent {
                key: c @ Arrow(_) | c @ Page(_) | c @ Home | c @ End,
                ..
            }) => self.move_cursor(c),
            // Ignore ESC
            Event::Key(KeyEvent { key: Esc, .. }) => {}

            Event::Key(KeyEvent { key: Chr(ch), .. }) => {
                self.insert_char(ch);
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::Drag(_),
                mut pos,
                ..
            }) => {
                let gutter_width = self.gutter_width()?;
                if !self.rows.is_empty() && pos.y < self.screen_size.row as usize {
                    if pos.y >= self.rows.len() {
                        pos.y = self.rows.len();
                        pos.x = 0;
                    } else {
                        pos.x = pos.x.min(self.rows[pos.y].len() + gutter_width);
                    }
                    self.highlight = Some((pos, pos));
                    log::trace!(target: "mouse", "Starting highlighting");
                }
                self.cur.x = pos.x.saturating_sub(gutter_width);
                self.cur.y = pos.y.min(self.rows.len());
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::Release(_),
                mut pos,
                ..
            }) => {
                let gutter_width = self.gutter_width()?;
                if let Some((_, end)) = self.highlight.as_mut() {
                    if pos.y >= self.rows.len() {
                        pos.y = self.rows.len();
                        pos.x = 0;
                    } else {
                        pos.x = pos.x.min(self.rows[pos.y].len() + gutter_width);
                    }
                    *end = pos;
                    log::trace!(target: "mouse", "Ended highlighting");
                }
                self.cur.x = pos.x.saturating_sub(gutter_width);
                self.cur.y = pos.y.min(self.rows.len());
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::Moved,
                mut pos,
                ..
            }) => {
                let gutter_width = self.gutter_width()?;
                if let Some((_, end)) = self.highlight.as_mut() {
                    if pos.y >= self.rows.len() {
                        pos.y = self.rows.len();
                        pos.x = 0;
                    } else {
                        pos.x = pos.x.min(self.rows[pos.y].len() + gutter_width);
                    }
                    *end = pos;
                }
                self.cur.x = pos.x.saturating_sub(gutter_width);
                self.cur.y = pos.y.min(self.rows.len());
            }
            Event::Mouse(MouseEvent {
                kind: MouseEventKind::Press(_),
                ..
            }) => {}
        }
        self.quit_count = QUIT_TIMES;
        Ok(())
    }

    fn move_cursor(&mut self, key: keys::Key) {
        let row = self.rows.get(self.cur.y);
        match key {
            keys::Key::Arrow(keys::Dir::Up) => self.cur.y = self.cur.y.saturating_sub(1),
            keys::Key::Arrow(keys::Dir::Down) => self.cur.y = (self.cur.y + 1).min(self.rows.len()),
            keys::Key::Arrow(keys::Dir::Left) => {
                if self.cur.x == 0 {
                    self.cur.y = self.cur.y.saturating_sub(1);
                    self.cur.x = self.rows.get(self.cur.y).map_or(0, |row| row.len());
                } else {
                    self.cur.x -= 1;
                }
            }
            keys::Key::Arrow(keys::Dir::Right) => {
                if self.cur.x == row.map_or(self.cur.x, |row| row.len()) {
                    self.cur.y = (self.cur.y + 1).min(self.rows.len());
                    self.cur.x = 0;
                } else {
                    self.cur.x += 1;
                }
            }
            keys::Key::Page(keys::Dir::Up) => {
                self.cur.y = self.cur.y.saturating_sub(self.screen_size.row as usize - 1)
            }
            keys::Key::Page(keys::Dir::Down) => {
                self.cur.y = (self.cur.y + self.screen_size.row as usize - 1).min(self.rows.len())
            }
            keys::Key::Home => self.cur.x = 0,
            keys::Key::End => self.cur.x = row.map_or(0, |row| row.len()),
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

    fn scroll_screen(&mut self) -> KResult<()> {
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
        let gutter_width = self.gutter_width()?;
        if self.render_pos_x >= self.coloff + (self.screen_size.col as usize - gutter_width) {
            self.coloff = self
                .render_pos_x
                .saturating_sub(self.screen_size.col as usize - gutter_width)
                + 1;
        }
        Ok(())
    }

    fn refresh_screen(&mut self) -> VoidResult {
        self.buf.clear();
        self.scroll_screen()?;
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

    /// Calculate width of the gutter where line numbers will be shown.
    fn gutter_width(&self) -> KResult<usize> {
        let gutter_width = if self.show_line_nums && !self.rows.is_empty() {
            2 + // '| ' separator width
            (self.rows.len().ilog10() as usize + 1)
        } else {
            0
        };
        if self.screen_size.col as usize <= gutter_width {
            Err(KError::SimpleMessage("Terminal is too narrow!"))
        } else {
            Ok(gutter_width)
        }
    }

    /// Write into `self.buf` the escape sequence needed to move cursor to `self.cur`.
    ///
    /// * Assumes `self.cur` is 0-indexed, whereas the terminal cursor needs to be 1-indexed.
    /// * Also transforms `self.cur.y` into `row` and `self.render_pos_x` into `col`.
    fn write_move_cur(&mut self) -> VoidResult {
        let gutter_width = self.gutter_width()?;
        write!(
            self.buf,
            "\x1b[{};{}H",
            self.cur.y - self.rowoff + 1,
            self.render_pos_x - self.coloff + 1 + gutter_width,
        )?;
        Ok(())
    }

    fn flush_buf(&mut self) -> VoidResult {
        self.stdout.write_all(self.buf.as_slice())?;
        Ok(())
    }

    fn draw_rows(&mut self) -> VoidResult {
        let gutter_width = self.gutter_width()?;
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
                if self.show_line_nums {
                    write!(
                        self.buf,
                        "{:>width$}| ",
                        filerow + 1,
                        width = gutter_width - 2
                    )?;
                }
                // only show the line if it is visible region
                if self.coloff < row.len() {
                    // need to clip manually. using std::fmt's width option causes line wraps.
                    let clip_end = row
                        .len()
                        .min(self.coloff + (self.screen_size.col as usize - gutter_width));
                    let clipped = &row[self.coloff..clip_end];
                    if let Some(hl) = self.highlight.as_ref() {
                        let (start, end) = hl.min_max();
                        if start.y < y && y < end.y {
                            // line is completely highlighted
                            esc_write!(self.buf, INVERT_COLOR)?;
                            write!(self.buf, "{clipped}")?;
                            esc_write!(self.buf, NORMAL_COLOR)?;
                        } else if start.y == y && end.y == y {
                            let split1 = start.x.saturating_sub(gutter_width);
                            let split2 = end.x.saturating_sub(gutter_width);
                            write!(self.buf, "{}", &clipped[..split1])?;
                            esc_write!(self.buf, INVERT_COLOR)?;
                            write!(self.buf, "{}", &clipped[split1..split2])?;
                            esc_write!(self.buf, NORMAL_COLOR)?;
                            write!(self.buf, "{}", &clipped[split2..])?;
                        } else if start.y == y {
                            let split = start.x.saturating_sub(gutter_width);
                            write!(self.buf, "{}", &clipped[..split])?;
                            esc_write!(self.buf, INVERT_COLOR)?;
                            write!(self.buf, "{}", &clipped[split..])?;
                            esc_write!(self.buf, NORMAL_COLOR)?;
                        } else if end.y == y {
                            let split = end.x.saturating_sub(gutter_width);
                            esc_write!(self.buf, INVERT_COLOR)?;
                            write!(self.buf, "{}", &clipped[..split])?;
                            esc_write!(self.buf, NORMAL_COLOR)?;
                            write!(self.buf, "{}", &clipped[split..])?;
                        } else {
                            // line is outside the selection range
                            write!(self.buf, "{clipped}")?;
                        }
                    } else {
                        write!(self.buf, "{clipped}")?;
                    }
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
            write!(
                self.buf,
                " - {} line{}",
                self.rows.len(),
                if self.rows.len() == 1 { "" } else { "s" }
            )?;
        }
        if self.dirty_count > 0 {
            write!(self.buf, " (modified)")?;
        }
        let end_len = self.buf.len();

        // format the rstatus into a buf first
        let rstatus: &mut [u8] = &mut [0u8; 40];
        let mut cur = std::io::Cursor::new(rstatus);
        write!(cur, "Ln{}, Col{}", self.cur.y + 1, self.cur.x + 1)?;
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
        }
        esc_write!(self.buf, CLEAR_TRAIL_LINE)?;
        Ok(())
    }

    fn set_status_message(&mut self, fmt_args: std::fmt::Arguments) {
        self.status_msg.0 = fmt_args.to_string();
        self.status_msg.1 = Instant::now();
    }

    fn prompt<F>(
        &mut self,
        prefix: &str,
        suffix: &str,
        mut callback: Option<F>,
    ) -> KResult<Option<String>>
    where
        F: FnMut(&mut Ked, &str, keys::Event) -> KResult<()>,
    {
        let mut user_inp = String::new();
        loop {
            self.set_status_message(format_args!("{prefix}{user_inp}{suffix}"));
            self.refresh_screen()?;
            let c = self.read_key()?;
            log::trace!(target: "keytrace::prompt", "Key {c:?}");
            match c {
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Enter,
                    ..
                }) => {
                    if !user_inp.is_empty() {
                        self.set_status_message(format_args!(""));
                        if let Some(callback) = callback.as_mut() {
                            callback(self, &user_inp, c)?
                        }

                        return Ok(Some(user_inp));
                    }
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Esc,
                    ..
                }) => {
                    // cancelled
                    if let Some(callback) = callback.as_mut() {
                        callback(self, &user_inp, c)?
                    }
                    return Ok(None);
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Chr('h'),
                    modifiers: keys::Modifier::Ctrl,
                })
                | keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Backspace,
                    ..
                }) => {
                    if !user_inp.is_empty() {
                        user_inp.truncate(user_inp.len() - 1)
                    };
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Chr(ch),
                    ..
                }) => {
                    if !ch.is_control() {
                        user_inp.push(ch);
                    }
                }
                _ => {
                    // ignore
                }
            }
            if let Some(callback) = callback.as_mut() {
                callback(self, &user_inp, c)?
            }
        }
    }

    fn get_render(&self, row_idx: usize) -> String {
        let line = &self.rows[row_idx];
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
    }

    fn insert_char_at_pos(&mut self, ch: char, row_idx: usize, col: usize) {
        let row = &mut self.rows[row_idx];
        log::trace!(target: "edit::insert", "Old row: '{row}'");
        if col > row.len() {
            log::warn!("Trying to insert char outside row!");
            log::debug!("Line: '{row}'. col: {col}");
            return;
        }
        row.reserve(1);
        row.insert(col, ch);
        log::trace!(target: "edit::insert", "New row: '{row}'");
        self.render_rows[row_idx] = self.get_render(row_idx);
    }

    fn insert_char(&mut self, ch: char) {
        log::trace!(target: "edit::insert", "Inserting '{ch}' at {:?}", self.cur);
        if self.cur.y >= self.rows.len() {
            let new_strs =
                std::iter::repeat_with(String::new).take(self.rows.len() - self.cur.y + 1);
            self.rows.extend(new_strs.clone());
            self.render_rows.extend(new_strs);
        }
        self.insert_char_at_pos(ch, self.cur.y, self.cur.x);
        self.cur.x += 1;
        self.dirty_count = self.dirty_count.saturating_add(1);
    }

    fn insert_newline(&mut self) {
        log::trace!(target: "edit::insert::newline", "Inserting nl at {:?}", self.cur);
        if self.cur.y >= self.rows.len() {
            let new_strs =
                std::iter::repeat_with(String::new).take(self.rows.len() - self.cur.y + 1);
            self.rows.extend(new_strs.clone());
            self.render_rows.extend(new_strs);
        } else {
            let at = self.cur.y;
            // insert a new blank row after current one
            if at == self.rows.len() - 1 {
                self.rows.push(String::new());
                self.render_rows.push(String::new());
            } else {
                self.rows.insert(at + 1, String::new());
                self.render_rows.insert(at + 1, String::new());
            }

            let [cur_row, next_row] = self
                .rows
                .get_many_mut([at, at + 1])
                .expect("Indices should be correct");
            let cut = self.cur.x.min(cur_row.len());
            if cut != cur_row.len() {
                next_row.insert_str(0, &cur_row[cut..]);
                cur_row.truncate(cut);
                self.render_rows[at] = self.get_render(at);
            }
            self.render_rows[at + 1] = self.get_render(at + 1);
        }
        self.cur.y += 1;
        self.cur.x = 0;
        self.dirty_count = self.dirty_count.saturating_add(1);
    }

    fn del_char_at_pos(&mut self, row_idx: usize, col: usize) {
        let row = &mut self.rows[row_idx];
        log::trace!(target: "edit::delete", "Old row: '{row}'");
        if col > row.len() {
            log::warn!("Trying to delete char outside row!");
            log::debug!("Line: '{row}'. col: {col}");
            return;
        }
        row.remove(col);
        log::trace!(target: "edit::delete", "New row: '{row}'");
        self.render_rows[row_idx] = self.get_render(row_idx);
        self.dirty_count = self.dirty_count.saturating_add(1);
    }

    /// Delete char that is to the left of the cursor.
    fn del_char(&mut self) {
        log::trace!(target: "edit::delete", "Deleting char at {:?}", self.cur);
        if self.cur.y >= self.rows.len() {
            // no need to do anything
            log::trace!(target: "edit::delete", "Deleting outside num_rows, no-op");
            return;
        }
        if self.cur.x == 0 {
            if self.cur.y == 0 {
                // no need to do anything
                log::trace!(target: "edit::delete", "Trying to delete at start of file, no-op");
                return;
            }
            log::trace!(target: "edit::delete", "Deleting at start of line");
            let cur_row = self.rows.remove(self.cur.y);
            self.render_rows.remove(self.cur.y);
            self.cur.y -= 1;
            let row = &mut self.rows[self.cur.y];
            self.cur.x = row.len();
            row.push_str(&cur_row);
            log::trace!(target: "edit::delete", "New row: '{row}'");
            self.render_rows[self.cur.y] = self.get_render(self.cur.y);
            self.dirty_count = self.dirty_count.saturating_add(1);
        } else {
            self.cur.x -= 1;
            self.del_char_at_pos(self.cur.y, self.cur.x);
        }
    }

    fn open(&mut self, path: impl AsRef<Path>) -> VoidResult {
        let f = std::fs::OpenOptions::new()
            .read(true)
            .write(false)
            .open(path.as_ref())?;
        let reader = BufReader::new(f);
        self.rows = reader.lines().collect::<Result<Vec<_>, _>>()?;
        self.render_rows = (0..self.rows.len())
            .map(|row_idx| self.get_render(row_idx))
            .collect();
        self.filepath = Some(path.as_ref().to_path_buf());
        log::trace!(
            "Opened file: {} with {} lines.",
            path.as_ref().display(),
            self.rows.len(),
        );
        Ok(())
    }

    fn save(&mut self) -> VoidResult {
        if self.filepath.is_none() {
            let path = self.prompt("Save as: ", " (ESC to cancel)", None::<PromptCB>)?;
            if let Some(path) = path {
                self.filepath = Some(PathBuf::from(path));
            } else {
                self.set_status_message(format_args!("Save cancelled."));
                return Ok(());
            }
        }
        let path = self.filepath.as_ref().unwrap();

        let file = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path);
        match file {
            Ok(mut file) => {
                let size = self.rows.iter().fold(0, |acc, r| r.len() + 1 + acc);
                let mut buf = Vec::with_capacity(size);
                for row in &self.rows {
                    writeln!(buf, "{row}")?;
                }
                match file.write_all(&buf) {
                    Ok(_) => {
                        self.set_status_message(format_args!("Wrote {size} bytes to disk"));
                        self.dirty_count = 0;
                    }
                    Err(err) => self
                        .set_status_message(format_args!("Error writing to file for save: {err}")),
                }
            }
            Err(err) => {
                self.set_status_message(format_args!("Error opening file for save: {err}"));
            }
        }
        Ok(())
    }

    fn find(&mut self) -> VoidResult {
        let saved_pos = self.cur;
        let coloff = self.coloff;
        let rowoff = self.rowoff;

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum SearchDir {
            Back,
            Forward,
        }

        // these will be captured by the callback
        let mut last_match: Option<Pos> = None;
        let mut direction = SearchDir::Forward;

        let callback = |ked: &mut Ked, query: &str, key: keys::Event| -> KResult<()> {
            log::trace!(target: "find::cb", "callback on '{query}' with {key:?}");
            match key {
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Esc,
                    ..
                }) => {
                    ked.set_status_message(format_args!("Search cancelled."));
                    return Ok(());
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Enter,
                    ..
                }) => {
                    return Ok(());
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Arrow(keys::Dir::Down | keys::Dir::Right),
                    ..
                }) => {
                    log::trace!(target: "find::cb", "searchdir forward");
                    direction = SearchDir::Forward;
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Arrow(keys::Dir::Up | keys::Dir::Left),
                    ..
                }) => {
                    log::trace!(target: "find::cb", "searchdir back");
                    direction = SearchDir::Back;
                }
                keys::Event::Key(keys::KeyEvent {
                    key: keys::Key::Chr(_),
                    ..
                }) => {
                    // got new char added to query, reset state
                    log::trace!(target: "find::cb", "reset last_match");
                    last_match = None;
                    direction = SearchDir::Forward;
                }
                _ => {
                    // ignore
                    return Ok(());
                }
            }
            let numrows = ked.rows.len();
            let rows_iter = ked.rows.iter().enumerate();
            let rows_iter = if direction == SearchDir::Forward {
                let skip_rows = last_match.map_or(0, |m| m.y);
                log::trace!(target: "find::cb", "forward skipped rows={skip_rows}");
                rows_iter // skip the rows before last_match, if it exists
                    .skip(skip_rows)
                    .enumerate()
                    .find_map(|(i, (rownum, row))| {
                        let find_offset = if i == 0 {
                            // find the match in the same row, just after last_match
                            last_match.map_or(0, |m| (m.x + 1).min(row.len()))
                        } else {
                            0
                        };
                        row[find_offset..].find(query).map(|x| Pos {
                            y: rownum,
                            x: x + find_offset,
                        })
                    })
            } else {
                let skip_rows = last_match.map_or(0, |m| numrows - m.y - 1);
                log::trace!(target: "find::cb", "back skipped rows={skip_rows}");
                rows_iter
                    .rev()
                    .skip(skip_rows)
                    .enumerate()
                    .find_map(|(i, (rownum, row))| {
                        let rfind_offset = if i == 0 {
                            // find the match in the same row, just before last_match
                            last_match.map_or(row.len(), |m| m.x + query.len() - 1)
                        } else {
                            row.len()
                        };
                        row[0..rfind_offset]
                            .rfind(query)
                            .map(|x| Pos { y: rownum, x })
                    })
            };
            if let Some(match_pos) = rows_iter {
                log::trace!(target: "find::cb", "found match {match_pos:?}");
                last_match = Some(match_pos);
                ked.cur = match_pos;
                ked.scroll_screen()?;
            }
            Ok(())
        };

        let query = self.prompt("Search: ", " (ESC to cancel)", Some(callback))?;
        if query.is_none() {
            // restore position if user cancelled the search
            self.cur = saved_pos;
            self.coloff = coloff;
            self.rowoff = rowoff;
            self.scroll_screen()?;
        }
        Ok(())
    }
}

impl Drop for Ked {
    fn drop(&mut self) {
        self.disable_mouse_events().unwrap_or_else(|e| {
            error!("Failed to disable mouse events: {e}");
        });
        self.clear_screen().unwrap_or_else(|e| {
            error!("Failed to clear screen on exit: {e}");
        });
        self.disable_raw_mode();
    }
}

fn run() -> VoidResult {
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
    ked.enable_mouse_events()?;
    if let Some(path) = std::env::args().nth(1) {
        ked.open(path)?;
    }
    ked.set_status_message(format_args!("HELP: Press Ctrl+q to quit"));
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

static BACKTRACE: std::sync::OnceLock<String> = std::sync::OnceLock::new();

fn panic_hook(_: &std::panic::PanicInfo) {
    let _ = BACKTRACE.set(std::backtrace::Backtrace::force_capture().to_string());
}

fn main() -> VoidResult {
    std::panic::set_hook(Box::new(panic_hook));
    std::panic::catch_unwind(run).unwrap_or_else(|e| {
        let panic_information = match e.downcast::<String>() {
            Ok(v) => *v,
            Err(e) => match e.downcast::<&str>() {
                Ok(v) => v.to_string(),
                _ => "Unknown Source of Error".to_owned(),
            },
        };

        log::error!("Panicked! {}", panic_information);
        if let Some(bt) = BACKTRACE.get() {
            log::error!("Backtrace:\n{bt}");
        }
        Ok(())
    })
}
