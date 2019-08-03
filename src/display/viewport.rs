use super::BoxStyle;
use super::DrawWithNeighbors;
use crate::display::draw_box::draw_box;
use crate::display::utils::clone_times;
use crate::entities::entity::Entity;
use crate::types::Neighbors;
use crate::types::{pos, BoundingBox, Direction, Position, Positioned};
use std::fmt::{self, Debug};
use std::io::{self, Write};

pub enum CursorState {
    Game,
    Prompt(Position),
}

impl Default for CursorState {
    fn default() -> Self {
        CursorState::Game
    }
}

pub struct Viewport<W> {
    /// The box describing the visible part of the viewport.
    ///
    /// Generally the size of the terminal, and positioned at 0, 0
    pub outer: BoundingBox,

    /// The box describing the game part of the viewport.
    pub game: BoundingBox,

    /// The box describing the inner part of the viewport
    ///
    /// Its position is relative to `outer.inner()`, and its size should
    /// generally not be smaller than outer
    pub inner: BoundingBox,

    /// The actual screen that the viewport writes to
    pub out: W,

    cursor_state: CursorState,

    /// Reset the cursor back to this position after every draw
    pub game_cursor_position: Position,
}

impl<W> Viewport<W> {
    pub fn new(outer: BoundingBox, inner: BoundingBox, out: W) -> Self {
        Viewport {
            outer,
            inner,
            out,
            game: outer.move_tr_corner(Position { x: 0, y: 1 }),
            cursor_state: Default::default(),
            game_cursor_position: pos(0, 0),
        }
    }

    /// Returns true if the (inner-relative) position of the given entity is
    /// visible within this viewport
    pub fn visible<E: Positioned>(&self, ent: &E) -> bool {
        self.on_screen(ent.position()).within(self.game.inner())
    }

    /// Convert the given inner-relative position to one on the actual screen
    fn on_screen(&self, pos: Position) -> Position {
        pos + self.inner.position + self.game.inner().position
    }
}

impl<W> Debug for Viewport<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Viewport {{ outer: {:?}, inner: {:?}, out: <OUT> }}",
            self.outer, self.inner
        )
    }
}

impl<W: Write> Viewport<W> {
    /// Draw the given entity to the viewport at its position, if visible
    #[allow(clippy::borrowed_box)]
    pub fn draw<T: DrawWithNeighbors>(
        &mut self,
        entity: &T,
        neighbors: &Neighbors<Vec<&Box<dyn Entity>>>,
    ) -> io::Result<()> {
        if !self.visible(entity) {
            return Ok(());
        }
        self.cursor_goto(entity.position())?;
        entity.do_draw_with_neighbors(self, neighbors)?;
        self.reset_cursor()
    }

    fn reset_cursor(&mut self) -> io::Result<()> {
        self.cursor_goto(self.game_cursor_position)
    }

    /// Move the cursor to the given inner-relative position
    pub fn cursor_goto(&mut self, pos: Position) -> io::Result<()> {
        write!(self, "{}", self.on_screen(pos).cursor_goto())
    }

    /// Clear whatever single character is drawn at the given inner-relative
    /// position, if visible
    pub fn clear(&mut self, pos: Position) -> io::Result<()> {
        write!(self, "{} ", self.on_screen(pos).cursor_goto(),)?;
        self.reset_cursor()
    }

    /// Initialize this viewport by drawing its outer box to the screen
    pub fn init(&mut self) -> io::Result<()> {
        draw_box(self, self.game, BoxStyle::Thin)
    }

    /// Write a message to the message area on the screen
    ///
    /// Will overwrite any message already present, and if the given message is
    /// longer than the screen will truncate. This means callers should handle
    /// message buffering and ellipsisization
    pub fn write_message(&mut self, msg: &str) -> io::Result<usize> {
        let msg_to_write = if msg.len() <= self.outer.dimensions.w as usize {
            msg
        } else {
            &msg[0..self.outer.dimensions.w as usize]
        };
        write!(
            self,
            "{}{}{}",
            self.outer.position.cursor_goto(),
            msg_to_write,
            clone_times::<_, String>(
                " ".to_string(),
                self.outer.dimensions.w - msg.len() as u16
            ),
        )?;
        self.reset_cursor()?;
        Ok(msg_to_write.len())
    }

    pub fn clear_message(&mut self) -> io::Result<()> {
        write!(
            self,
            "{}{}",
            self.outer.position.cursor_goto(),
            clone_times::<_, String>(
                " ".to_string(),
                self.outer.dimensions.w as u16
            )
        )?;
        self.reset_cursor()
    }

    /// Write a prompt requesting text input to the message area on the screen.
    ///
    /// Will overwrite any message already present, and if the given message is
    /// longer than the screen will truncate. This means callers should handle
    /// message buffering and ellipsisization
    pub fn write_prompt<'a, 'b>(&'a mut self, msg: &'b str) -> io::Result<()> {
        let len = self.write_message(msg)? + 1;
        let pos = self.outer.position + pos(len as i16, 0);
        self.cursor_state = CursorState::Prompt(pos);
        write!(self, "{}", pos.cursor_goto())?;
        self.flush()
    }

    pub fn push_prompt_chr(&mut self, chr: char) -> io::Result<()> {
        if let CursorState::Prompt(pos) = self.cursor_state {
            write!(self, "{}", chr)?;
            self.cursor_state = CursorState::Prompt(pos + Direction::Right);
        }
        Ok(())
    }

    pub fn pop_prompt_chr(&mut self) -> io::Result<()> {
        if let CursorState::Prompt(pos) = self.cursor_state {
            let new_pos = pos + Direction::Left;
            write!(
                self,
                "{} {}",
                new_pos.cursor_goto(),
                new_pos.cursor_goto()
            )?;
            self.cursor_state = CursorState::Prompt(new_pos);
        }
        Ok(())
    }

    pub fn clear_prompt(&mut self) -> io::Result<()> {
        self.clear_message()?;
        self.cursor_state = CursorState::Game;
        Ok(())
    }
}

impl<W> Positioned for Viewport<W> {
    fn position(&self) -> Position {
        self.outer.position
    }
}

impl<W: Write> Write for Viewport<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.out.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.out.flush()
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.out.write_all(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Dimensions;
    // use proptest::prelude::*;

    #[test]
    fn test_visible() {
        assert!(Viewport::new(
            BoundingBox::at_origin(Dimensions { w: 10, h: 10 }),
            BoundingBox {
                position: Position { x: -10, y: -10 },
                dimensions: Dimensions { w: 15, h: 15 },
            },
            ()
        )
        .visible(&Position { x: 13, y: 13 }));

        assert!(!Viewport::new(
            BoundingBox::at_origin(Dimensions { w: 10, h: 10 }),
            BoundingBox {
                position: Position { x: -10, y: -10 },
                dimensions: Dimensions { w: 15, h: 15 },
            },
            (),
        )
        .visible(&Position { x: 1, y: 1 }));
    }

    // proptest! {
    //     #[test]
    //     fn nothing_is_visible_in_viewport_off_screen(pos: Position, outer: BoundingBox) {
    //         let invisible_viewport = Viewport {
    //             outer,
    //             inner: BoundingBox {
    //                 position: Position {x: -(outer.dimensions.w as i16), y: -(outer.dimensions.h as i16)},
    //                 dimensions: outer.dimensions,
    //             },
    //             out: ()
    //         };

    //         assert!(!invisible_viewport.visible(&pos));
    //     }
    // }
}
