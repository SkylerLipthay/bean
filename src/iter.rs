use crate::position::Position;
use std::collections::VecDeque;

pub struct CharsWithPosition<I: Iterator<Item = char>> {
    iter: MultiPeek<I>,
    prev_pos: Position,
    next_pos: Position,
}

impl<I: Iterator<Item = char>> CharsWithPosition<I> {
    pub fn new(iter: I) -> CharsWithPosition<I> {
        CharsWithPosition {
            iter: MultiPeek::new(iter),
            prev_pos: Position { line: 1, column: 0 },
            next_pos: Position { line: 1, column: 1 },
        }
    }

    pub fn peek(&mut self, index: usize) -> Option<&I::Item> {
        self.iter.peek(index)
    }

    pub fn prev_pos(&self) -> Position {
        self.prev_pos
    }

    pub fn next_pos(&self) -> Position {
        self.next_pos
    }
}

impl<I: Iterator<Item = char>> Iterator for CharsWithPosition<I> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.prev_pos = self.next_pos;
        match self.iter.next() {
            Some('\n') => {
                self.next_pos.column = 1;
                self.next_pos.line += 1;
                Some('\n')
            },
            Some(c) => {
                self.next_pos.column += 1;
                Some(c)
            },
            None => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct MultiPeek<I: Iterator> {
    iter: I,
    buf: VecDeque<I::Item>,
}

impl<I: Iterator> MultiPeek<I> {
    pub fn new(iter: I) -> MultiPeek<I> {
        MultiPeek {
            iter,
            buf: VecDeque::new(),
        }
    }
}

impl<I: Iterator> MultiPeek<I> {
    pub fn peek(&mut self, index: usize) -> Option<&I::Item> {
        if index < self.buf.len() {
            return Some(&self.buf[index]);
        }

        for _ in 0..=(index - self.buf.len()) {
            match self.iter.next() {
                Some(x) => self.buf.push_back(x),
                None => break,
            }
        }

        self.buf.get(index)
    }

    pub fn inner(&self) -> &I {
        &self.iter
    }
}

impl<I: Iterator> Iterator for MultiPeek<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        if self.buf.is_empty() {
            self.iter.next()
        } else {
            self.buf.pop_front()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chars_with_position() {
        macro_rules! assert_chars_with_pos {
            ($iter:expr, $val:expr, $prev_pos:expr, $next_pos:expr) => {
                assert_eq!($iter.next(), $val);
                assert_eq!($iter.prev_pos(), Position { line: $prev_pos.0, column: $prev_pos.1 });
                assert_eq!($iter.next_pos(), Position { line: $next_pos.0, column: $next_pos.1 });
            }
        }

        let mut iter = CharsWithPosition::new("abc\ndef".chars());
        assert_chars_with_pos!(iter, Some('a'), (1, 1), (1, 2));
        assert_chars_with_pos!(iter, Some('b'), (1, 2), (1, 3));
        assert_chars_with_pos!(iter, Some('c'), (1, 3), (1, 4));
        assert_chars_with_pos!(iter, Some('\n'), (1, 4), (2, 1));
        assert_chars_with_pos!(iter, Some('d'), (2, 1), (2, 2));
        assert_chars_with_pos!(iter, Some('e'), (2, 2), (2, 3));
        assert_chars_with_pos!(iter, Some('f'), (2, 3), (2, 4));
        assert_chars_with_pos!(iter, None, (2, 4), (2, 4));
    }

    #[test]
    fn multi_peek() {
        let mut iter = MultiPeek::new(vec![0, 1, 2, 3, 4].into_iter());
        assert_eq!(iter.peek(0), Some(&0));
        assert_eq!(iter.peek(0), Some(&0));
        assert_eq!(iter.peek(2), Some(&2));
        assert_eq!(iter.peek(2), Some(&2));
        assert_eq!(iter.peek(1), Some(&1));
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.peek(1), Some(&3));
        assert_eq!(iter.peek(0), Some(&2));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.peek(0), Some(&3));
        assert_eq!(iter.peek(1), Some(&4));
        assert_eq!(iter.peek(2), None);
        assert_eq!(iter.peek(3), None);
        assert_eq!(iter.peek(4), None);
        assert_eq!(iter.peek(1), Some(&4));
        assert_eq!(iter.peek(0), Some(&3));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
    }
}
