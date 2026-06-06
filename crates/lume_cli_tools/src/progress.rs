use std::borrow::Cow;

use crate::{ACCENT_ACCENT, Stylable};

pub const DEFAULT_GUTTER_SIZE: usize = 12;

pub struct ProgressBar {
    gutter_size: usize,
    inner: indicatif::ProgressBar,
}

impl ProgressBar {
    pub fn new(length: usize) -> Self {
        Self::no_length().with_length(length)
    }

    pub fn no_length() -> Self {
        Self {
            gutter_size: 0,
            inner: indicatif::ProgressBar::no_length(),
        }
        .with_gutter(DEFAULT_GUTTER_SIZE)
    }

    /// Sets the current gutter size of the progress bar.
    pub fn with_gutter(mut self, gutter_size: usize) -> Self {
        self.inner.set_style(
            indicatif::ProgressStyle::with_template(&format!(
                "{{prefix:>{gutter_size}.{ACCENT_ACCENT}.bold}} [{{bar:57}}] {{pos}}/{{len}} {{wide_msg}}"
            ))
            .unwrap()
            .progress_chars("-> "),
        );

        self.gutter_size = gutter_size;
        self
    }

    /// Sets the current length of the progress bar.
    pub fn with_length(self, length: usize) -> Self {
        self.inner.set_length(length as u64);
        self
    }

    /// Sets the current prefix of the progress bar.
    pub fn with_prefix<P: Into<Cow<'static, str>>>(self, prefix: P) -> Self {
        self.inner.set_prefix(prefix);
        self
    }

    pub fn hidden(self) -> Self {
        self.inner.set_draw_target(indicatif::ProgressDrawTarget::hidden());
        self
    }

    pub fn hide(&self) {
        self.inner.set_draw_target(indicatif::ProgressDrawTarget::hidden());
        self.inner.force_draw();
    }

    pub fn show(&self) {
        self.inner.set_draw_target(indicatif::ProgressDrawTarget::stdout());
        self.inner.force_draw();
    }

    /// Sets the current length of the progress bar.
    pub fn set_length(&self, length: u64) {
        self.inner.set_length(length);
    }

    /// Sets the current prefix of the progress bar.
    pub fn set_prefix<P: Into<Cow<'static, str>>>(&self, prefix: P) {
        self.inner.set_prefix(prefix);
    }

    pub fn inc(&self, delta: u64) {
        self.inner.inc(delta);
    }

    pub fn println<G: AsRef<str>, M: AsRef<str>>(&self, gutter: G, message: M) {
        self.inner.println(format!(
            "{:>gutter$} {}",
            gutter.as_ref().stylize("primary.bold"),
            message.as_ref(),
            gutter = self.gutter_size
        ));
    }

    pub fn finish(&self) {
        self.inner.finish_and_clear();
    }

    pub fn finish_with<G: AsRef<str>, M: AsRef<str>>(&self, gutter: G, message: M) {
        self.println(gutter, message);
        self.finish();
    }
}
