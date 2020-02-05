// derived from the itertools source.

use std::iter::Peekable;

#[derive(Debug, Clone)]
pub struct PutBackN<I: Iterator> {
    top: Vec<I::Item>,
    iter: Peekable<I>,
}

pub fn put_back_n<I>(iterable: I) -> PutBackN<I::IntoIter>
    where I: IntoIterator
{
    PutBackN {
        top: Vec::new(),
        iter: iterable.into_iter().peekable()
    }
}

impl<I: Iterator> PutBackN<I> {
    #[inline]
    pub fn put_back(&mut self, item: I::Item) {
        self.top.push(item);
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&I::Item> {
        if self.top.is_empty() {
            self.iter.peek()
        } else {
            self.top.last()
        }
    }
}

impl<I: Iterator> Iterator for PutBackN<I> {
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<I::Item> {
        if self.top.is_empty() {
            self.iter.next()
        } else {
            self.top.pop()
        }
    }
}
