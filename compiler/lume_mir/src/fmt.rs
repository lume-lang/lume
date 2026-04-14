pub struct WithFlags<'a, T> {
    pub value: &'a T,
    pub alternate: bool,
}

impl<T: std::fmt::Display> std::fmt::Display for WithFlags<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.alternate {
            write!(f, "{:#}", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

pub(crate) fn with_flags<'a, T: std::fmt::Display>(f: &std::fmt::Formatter<'_>, value: &'a T) -> WithFlags<'a, T> {
    WithFlags {
        value,
        alternate: f.alternate(),
    }
}
