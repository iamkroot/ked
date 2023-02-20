pub(crate) trait MinMax {
    fn min_max(self) -> Self;
}

impl<T: Copy + PartialOrd + Ord> MinMax for (T, T) {
    #[inline]
    fn min_max(self) -> Self {
        if self.0 <= self.1 {
            (self.0, self.1)
        } else {
            (self.1, self.0)
        }
    }
}
