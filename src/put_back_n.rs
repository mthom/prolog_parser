
#[derive(Debug, Clone)]
pub struct PutBackN<I: Iterator> {
    top: Vec<I::Item>,
    iter: I,
}

pub fn put_back_n<I>(iterable: I) -> PutBackN<I::IntoIter>
    where I: IntoIterator
{
    PutBackN {
        top: Vec::new(),
        iter: iterable.into_iter(),
    }
}

impl<I: Iterator> PutBackN<I> {
    #[inline]
    pub(crate)
    fn put_back(&mut self, item: I::Item) {
        self.top.push(item);
    }

    #[inline]
    pub fn take_buf(&mut self) -> Vec<I::Item> {
        std::mem::replace(&mut self.top, vec![])
    }

    #[inline]
    pub fn take_iter_mut(&mut self) -> &mut I {
        &mut self.iter
    }

    #[inline]
    pub(crate)
    fn peek(&mut self) -> Option<&I::Item> {
        if self.top.is_empty() {
            match self.iter.next() {
                Some(item) => {
                    self.top.push(item);
                    self.top.last()
                }
                None => {
                    None
                }
            }
        } else {
            self.top.last()
        }
    }

    #[inline]
    pub(crate)
    fn put_back_all<DEI: DoubleEndedIterator<Item = I::Item>>(&mut self, iter: DEI) {
        self.top.extend(iter.rev());
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
