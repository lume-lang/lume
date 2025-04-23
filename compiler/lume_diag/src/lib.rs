use crate::source::*;
use annotate_snippets::{Level, Message, Renderer, Snippet};
use std::ops::Range;

pub mod handler;
pub mod source;

pub type Error = Box<dyn Diagnostic + Send + Sync>;

pub type Result<T> = std::result::Result<T, Error>;

/// Diagnostic severity level.
///
/// Intended to be used by the reporter to change how the diagnostic is displayed.
/// Diagnostics of [`Error`] or higher also cause the reporter to halt upon draining.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Failure. Program cannot continue.
    #[default]
    Error,

    /// Warning. Program can continue but may be affected.
    Warning,

    /// Information. Program can continue and may be unaffected.
    Info,

    /// Note. Has no effect on the program, but may provide additional context.
    Note,

    /// Help. Has no effect on the program, but may provide extra help and tips.
    Help,
}

impl From<Severity> for Level {
    fn from(val: Severity) -> Level {
        match val {
            Severity::Error => Level::Error,
            Severity::Warning => Level::Warning,
            Severity::Info => Level::Info,
            Severity::Note => Level::Note,
            Severity::Help => Level::Help,
        }
    }
}

/// Defines some span within a [`Source`] instance.
///
/// The range within the span is an absolute zero-indexed range of characters within the source file.
/// It is not a line-column representation and does not provide information about the line and column numbers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanRange(pub Range<usize>);

impl From<Range<usize>> for SpanRange {
    fn from(range: Range<usize>) -> SpanRange {
        SpanRange(Range {
            start: range.start,
            end: range.end,
        })
    }
}

/// Represents some line within a [`Source`] instance.
///
/// The line number is zero-indexed and represents the line number within the source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanLine(pub usize);

impl SpanLine {
    /// Determines the line number, where the given span is located.
    pub fn from_source(source: &dyn Source, range: &SpanRange) -> Self {
        let mut line = 0usize;

        for (index, char) in source.content().chars().enumerate() {
            if index >= range.0.start {
                break;
            }

            if char == '\n' {
                line += 1;
            }
        }

        SpanLine(line)
    }
}

impl From<usize> for SpanLine {
    fn from(line: usize) -> Self {
        SpanLine(line)
    }
}

/// Represents a labelled span of some source code.
///
/// Each label is meant to be used as a snippet within a larger source code. It provides
/// a way to highlight a specific portion of the source code, and uses labels to provide
/// additional information about the span.
#[derive(Debug)]
pub struct Label<'a> {
    /// Defines the actual label to print on the snippet.
    label: String,

    /// Defines the source span where the label should be placed.
    source: &'a dyn Source,

    /// Defines the index range where the label should be placed.
    range: SpanRange,
}

impl<'a> Label<'a> {
    /// Creates a new [`Span`] from the given source, range, and label.
    pub fn new(source: &'a dyn Source, range: impl Into<SpanRange>, label: String) -> Self {
        Self {
            source,
            range: range.into(),
            label,
        }
    }

    /// Creates a new [`annotate_snippets::Snippet`] from the span, which
    /// can then be reported to the user via the [`annotate_snippets::renderer::Renderer`] class.
    pub fn into_snippet(&'a self, level: Level) -> Snippet<'a> {
        let SpanLine(line) = SpanLine::from_source(self.source, &self.range);
        let snipped_source_indices = self.source_snippet(self.source);

        let mut annotation_span = self.range.0.clone();
        annotation_span.start -= snipped_source_indices.start;
        annotation_span.end -= snipped_source_indices.start;

        let annotation = level.span(annotation_span).label(&self.label);
        let snipped_source = &self.source.content()[snipped_source_indices];

        let mut snippet = Snippet::source(snipped_source).line_start(line).annotation(annotation);

        if let Some(name) = &self.source.name() {
            snippet = snippet.origin(name);
        }

        snippet
    }

    /// Gets the indices of the snippet within the source file.
    ///
    /// This method will return the indices of the source code, which the span is pointing to,
    /// as well as the line immediately before and after the snippet. The indices will point to the
    /// start and end of each line.
    fn source_snippet(&'a self, source: &'a dyn Source) -> Range<usize> {
        let bytes = source.content().as_bytes();

        let start = self.range.0.start;
        let end = self.range.0.end;

        // Find the start of the line containing the `start`
        let mut line_start = start;
        while line_start > 0 && bytes[line_start - 1] != b'\n' {
            line_start -= 1;
        }

        // Find the end of the line containing the `end`
        let mut line_end = end;
        while line_end < bytes.len() && bytes[line_end] != b'\n' {
            line_end += 1;
        }

        // Now find the line before
        let mut before_start = line_start;
        if before_start > 0 {
            before_start -= 1; // move before the newline
            while before_start > 0 && bytes[before_start - 1] != b'\n' {
                before_start -= 1;
            }
        }

        // Find the line after
        let mut after_end = line_end;
        if after_end < bytes.len() {
            after_end += 1; // skip the newline
            while after_end < bytes.len() && bytes[after_end] != b'\n' {
                after_end += 1;
            }
        }

        before_start..after_end
    }
}

pub trait Diagnostic: std::fmt::Debug {
    /// Defines which message to be raised to the user, when reported.
    fn message(&self) -> String;

    /// Diagnostic severity level.
    ///
    /// This may be used by the renderer to determine how to display the diagnostic or
    /// even halt the program, depending on the severity level.
    fn severity(&self) -> Severity {
        Severity::default()
    }

    /// Unique diagnostic code, which can be used to look up more information about the error.
    fn code(&self) -> Option<&str> {
        None
    }

    /// Source span to attach labels to.
    fn span(&self) -> Option<&dyn Source> {
        None
    }

    /// Labels to attach to snippets of the source code.
    fn labels<'a>(&'a self) -> Option<Vec<Box<Label<'a>>>> {
        None
    }

    /// Defines the cause of the diagnostic, if any.
    fn source(&self) -> Option<&dyn std::error::Error> {
        None
    }

    /// Help messages, which can be used to provide additional information about the diagnostic.
    fn help(&self) -> Option<Vec<String>> {
        None
    }

    /// Turns the current diagnostic into a [`LumeDiagnostic`] object.
    fn as_diag<'a>(&'a self) -> LumeDiagnostic<'a> {
        LumeDiagnostic {
            message: self.message(),
            severity: self.severity(),
            code: self.code(),
            labels: self.labels(),
            source: self.source(),
            help: self.help(),
        }
    }
}

impl<T: Diagnostic + Send + Sync + 'static> From<T> for Box<dyn Diagnostic + Send + Sync + 'static> {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}

impl<T: Diagnostic + Send + Sync + 'static> From<T> for Box<dyn Diagnostic + Send + 'static> {
    fn from(value: T) -> Self {
        Box::<dyn Diagnostic + Send + Sync>::from(value)
    }
}

impl<T: Diagnostic + Send + Sync + 'static> From<T> for Box<dyn Diagnostic + 'static> {
    fn from(value: T) -> Self {
        Box::<dyn Diagnostic + Send + Sync>::from(value)
    }
}

impl From<Box<dyn std::error::Error + Send + Sync>> for Box<dyn Diagnostic + Send + Sync> {
    fn from(s: Box<dyn std::error::Error + Send + Sync>) -> Self {
        #[derive(Debug)]
        struct BoxedDiagnostic(Box<dyn std::error::Error + Send + Sync>);

        impl Diagnostic for BoxedDiagnostic {
            fn message(&self) -> String {
                self.0.to_string()
            }
        }

        Box::new(BoxedDiagnostic(s))
    }
}

impl From<std::io::Error> for Box<dyn Diagnostic + Send + Sync> {
    fn from(s: std::io::Error) -> Self {
        From::<Box<dyn std::error::Error + Send + Sync>>::from(Box::new(s))
    }
}

impl std::cmp::PartialEq for Box<dyn Diagnostic + Send + Sync> {
    fn eq(&self, other: &Self) -> bool {
        self.message() == other.message()
    }
}

impl std::cmp::Eq for Box<dyn Diagnostic + Send + Sync> {}

/// Adds metadata to some error or diagnostic, which should be reported to the user.
///
/// This struct is used to provide additional information about an error or diagnostic,
/// such as a code, a source span, and help messages.
///
/// # Examples
///
/// ```
/// use lume_diag::LumeDiagnostic;
///
/// let diagnostic = LumeDiagnostic::new("An error occurred".into());
///
/// diagnostic.render();
/// ```
#[derive(Debug)]
pub struct LumeDiagnostic<'a> {
    /// Defines which message to be raised to the user, when reported.
    pub message: String,

    /// Diagnostic severity level.
    ///
    /// This may be used by the renderer to determine how to display the diagnostic or
    /// even halt the program, depending on the severity level.
    pub severity: Severity,

    /// Unique diagnostic code, which can be used to look up more information about the error.
    pub code: Option<&'a str>,

    /// Labels to attach to snippets of the source code.
    pub labels: Option<Vec<Box<Label<'a>>>>,

    /// Chained error, which caused the diagnostic to be raised.
    pub source: Option<&'a dyn std::error::Error>,

    /// Help messages, which can be used to provide additional information about the diagnostic.
    pub help: Option<Vec<String>>,
}

impl<'a> LumeDiagnostic<'a> {
    /// Creates a new diagnostic with the given message.
    ///
    /// The created diagnostic will have no other context attached, such as source code, help
    /// messages, or error code. By default, the severity level is set to `Error`.
    ///
    /// Using this method is effectively the same as:
    ///
    /// ```ignore
    /// use diag::LumeDiagnostic;
    /// use diag::Severity;
    ///
    /// LumeDiagnostic {
    ///     message,
    ///     severity: Severity::Error,
    ///     code: None,
    ///     labels: None,
    ///     source: None,
    ///     help: None,
    /// }
    /// ```
    pub fn new(message: String) -> Self {
        Self {
            message,
            severity: Severity::default(),
            code: None,
            labels: None,
            source: None,
            help: None,
        }
    }

    /// Sets the severity level of the diagnostic.
    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }

    /// Sets the unique code of the diagnostic.
    pub fn with_code(mut self, code: impl Into<&'a str>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Sets the source error of the diagnostic.
    pub fn with_source(mut self, error: &'a dyn std::error::Error) -> Self {
        self.source = Some(error);
        self
    }

    /// Adds the given label to the diagnostic.
    pub fn add_label(mut self, label: Label<'a>) -> Self {
        if self.labels.is_none() {
            self.labels = Some(Vec::new());
        }

        self.labels.as_mut().unwrap().push(Box::new(label));

        self
    }

    /// Adds the given labels to the diagnostic.
    pub fn add_labels(mut self, labels: impl IntoIterator<Item = Label<'a>>) -> Self {
        if self.labels.is_none() {
            self.labels = Some(Vec::new());
        }

        let labels = labels.into_iter().map(Box::new).collect::<Vec<Box<Label<'a>>>>();

        self.labels.as_mut().unwrap().extend(labels);

        self
    }

    /// Adds the given help message to the diagnostic.
    pub fn add_help(mut self, help: String) -> Self {
        if self.help.is_none() {
            self.help = Some(Vec::new());
        }

        self.help.as_mut().unwrap().push(help);

        self
    }

    /// Appends the given help messages to the diagnostic.
    pub fn append_help(mut self, help: impl IntoIterator<Item = String>) -> Self {
        if self.help.is_none() {
            self.help = Some(Vec::new());
        }

        self.help.as_mut().unwrap().extend(help);

        self
    }

    /// Renders the diagnostic message to the console.
    ///
    /// By default, the "style" rendered is used for rendering. To use a different renderer,
    /// see the [`render_with`] method.
    pub fn render(&'a self) {
        self.render_with(Renderer::styled());
    }

    /// Renders the diagnostic message to the console, using a custom renderer implementation.
    pub fn render_with(&'a self, renderer: Renderer) {
        let message = self.get_message();

        println!("{}", renderer.render(message));
    }

    /// Converts the diagnostic into a [`annotate_snippets::Message`] instance.
    fn get_message(&'a self) -> Message<'a> {
        let level: Level = self.severity.into();

        let mut message = level.title(&self.message);

        if let Some(code) = &self.code {
            message = message.id(code);
        }

        if let Some(labels) = &self.labels {
            for label in labels {
                message = message.snippet(label.into_snippet(level));
            }
        }

        if let Some(source) = &self.source {
            let source_desc = source.to_owned().to_string();

            // While not exactly ideal, I'm unsure how else to handle the shorter
            // lifetime of the error description string.
            let source_desc_static: &'static str = Box::leak(source_desc.into_boxed_str());

            message = message.footer(Level::Error.title(source_desc_static));
        }

        if let Some(help) = &self.help {
            for line in help {
                message = message.footer(Level::Help.title(line));
            }
        }

        message
    }
}

impl<'a> std::error::Error for LumeDiagnostic<'a> {}

impl<'a> std::fmt::Display for LumeDiagnostic<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
