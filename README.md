> Everybody writes a screen editor. It's easy to do and makes them feel important. Tell them to work on something useful.

— Rob Pike ([source](https://news.ycombinator.com/item?id=20607261))

> _Tell them to work on something useful._

...
Nah.

# ked
A text editor, for fun and knowledge.

Based on the excellent [Build Your Own Text Editor](https://viewsourcecode.org/snaptoken/kilo/) series.
Most of the current code is a direct port of this tutorial.

## TODOs
A whimsical list of improvements I'd like to implement-
- [x] Line numbers
- [x] Mouse events
    - [x] Simple cursor movement
    - [x] Drag to highlight
- [ ] Cursor position history ([this](https://austinhenley.com/blog/images/textcursor.gif))
- [ ] Undo and redo
- [ ] Cut, copy, and paste
- [ ] Represent modifications using a [Piece Table](https://en.wikipedia.org/wiki/Piece_table) data structure
- [ ] Word wrapping (Rust's `std::fmt` should make this easy)
