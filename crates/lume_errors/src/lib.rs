use std::fmt::Display;
use std::ops::Range;
use std::sync::Arc;

pub mod context;
pub mod handler;
pub mod render;
pub mod source;

pub use crate::context::*;
pub use crate::handler::*;
pub use crate::render::*;
pub use crate::source::*;

pub type Error = Box<dyn Diagnostic + Send + Sync>;

pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! diagnostic {
    ($($arg:tt)*) => {
        $crate::SimpleDiagnostic::new(format!($($arg)*))
    };
}

/// Diagnostic severity level.
///
/// Intended to be used by the reporter to change how the diagnostic is
/// displayed. Diagnostics of [`Error`] or higher also cause the reporter to
/// halt upon draining.
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

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => f.write_str("error"),
            Severity::Warning => f.write_str("warning"),
            Severity::Info => f.write_str("info"),
            Severity::Note => f.write_str("note"),
            Severity::Help => f.write_str("help"),
        }
    }
}

/// Defines some span within a [`Source`] instance.
///
/// The range within the span is an absolute zero-indexed range of characters
/// within the source file. It is not a line-column representation and does not
/// provide information about the line and column numbers.
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

impl From<SpanRange> for Range<usize> {
    fn from(span: SpanRange) -> Range<usize> {
        span.0
    }
}

impl std::fmt::Display for SpanRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

/// Defines some location with a [`Source`] instance.
///
/// The location within the structure is an absolute zero-indexed position of
/// characters within the source file. It is not a line-column representation
/// and does not provide information about the line and column numbers.
#[derive(Debug, Clone)]
pub struct SourceLocation {
    /// Defines the source which the range is referring to.
    source: Arc<dyn Source>,

    /// Defines the character offset into the file.
    offset: usize,
}

impl SourceLocation {
    /// Creates a new [`SourceLocation`] with the given source and offset.
    pub fn new(source: Arc<dyn Source>, offset: usize) -> Self {
        Self { source, offset }
    }
}

impl PartialEq for SourceLocation {
    fn eq(&self, other: &Self) -> bool {
        self.source.name() == other.source.name()
            && self.source.content() == other.source.content()
            && self.offset == other.offset
    }
}

impl std::cmp::Eq for SourceLocation {}

impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, other: &SourceLocation) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for SourceLocation {
    fn cmp(&self, other: &SourceLocation) -> std::cmp::Ordering {
        let other_offset = other.offset;

        match self.offset {
            v if v < other_offset => std::cmp::Ordering::Less,
            v if v > other_offset => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        }
    }
}

/// Defines some span with a [`Source`] instance.
///
/// The range within the span is an absolute zero-indexed range of characters
/// within the source file. It is not a line-column representation and does not
/// provide information about the line and column numbers.
#[derive(Debug, Clone)]
pub struct SourceRange {
    /// Defines the source which the range is referring to.
    source: Arc<dyn Source>,

    /// Defines the underlying span.
    span: SpanRange,
}

impl SourceRange {
    /// Creates a new [`SourceRange`] with the given source and span.
    pub fn new(source: Arc<dyn Source>, span: impl Into<SpanRange>) -> Self {
        Self {
            source,
            span: span.into(),
        }
    }
}

impl PartialEq for SourceRange {
    fn eq(&self, other: &Self) -> bool {
        self.source.name() == other.source.name()
            && self.source.content() == other.source.content()
            && self.span == other.span
    }
}

impl std::cmp::Eq for SourceRange {}

impl PartialOrd for SourceRange {
    fn partial_cmp(&self, other: &SourceRange) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for SourceRange {
    fn cmp(&self, other: &SourceRange) -> std::cmp::Ordering {
        let other_start = other.span.0.start;

        match self.span.0.start {
            v if v < other_start => std::cmp::Ordering::Less,
            v if v > other_start => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        }
    }
}

/// Represents a labelled span of some source code.
///
/// Each label is meant to be used as a snippet within a larger source code. It
/// provides a way to highlight a specific portion of the source code, and uses
/// labels to provide additional information about the span.
#[derive(Debug, Clone)]
pub struct Label {
    /// Defines the actual label to print on the snippet.
    message: String,

    /// Defines the source span where the label should be placed.
    ///
    /// If this method returns `None`, the parent diagnostic is expected to have
    /// a source attached via the [`Diagnostic::source_code()`] method.
    source: Option<Arc<dyn Source>>,

    /// Defines the index range where the label should be placed.
    range: SpanRange,

    /// Defines the severity of the label, which can be independant from the
    /// parent diagnostic.
    severity: Option<Severity>,
}

impl PartialEq for Label {
    fn eq(&self, other: &Label) -> bool {
        self.message == other.message && self.range == other.range
    }
}

impl Eq for Label {}

impl Label {
    /// Creates a new [`Label`] from the given source, range, and label.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), None);
    /// ```
    pub fn new(range: impl Into<SpanRange>, message: impl Into<String>) -> Self {
        Self {
            source: None,
            range: range.into(),
            message: message.into(),
            severity: None,
        }
    }

    /// Creates a new [`Label`] from the given source, range, and label, with a
    /// severity of [`Severity::Error`].
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::error(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), Some(Severity::Error));
    /// ```
    pub fn error(range: impl Into<SpanRange>, label: impl Into<String>) -> Self {
        Self {
            source: None,
            range: range.into(),
            message: label.into(),
            severity: Some(Severity::Error),
        }
    }

    /// Creates a new [`Label`] from the given source, range, and label, with a
    /// severity of [`Severity::Warning`].
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::warning(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), Some(Severity::Warning));
    /// ```
    pub fn warning(range: impl Into<SpanRange>, label: impl Into<String>) -> Self {
        Self {
            source: None,
            range: range.into(),
            message: label.into(),
            severity: Some(Severity::Warning),
        }
    }

    /// Creates a new [`Label`] from the given source, range, and label, with a
    /// severity of [`Severity::Info`].
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::info(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), Some(Severity::Info));
    /// ```
    pub fn info(range: impl Into<SpanRange>, label: impl Into<String>) -> Self {
        Self {
            source: None,
            range: range.into(),
            message: label.into(),
            severity: Some(Severity::Info),
        }
    }

    /// Creates a new [`Label`] from the given source, range, and label, with a
    /// severity of [`Severity::Note`].
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::note(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), Some(Severity::Note));
    /// ```
    pub fn note(range: impl Into<SpanRange>, label: impl Into<String>) -> Self {
        Self {
            source: None,
            range: range.into(),
            message: label.into(),
            severity: Some(Severity::Note),
        }
    }

    /// Creates a new [`Label`] from the given source, range, and label, with a
    /// severity of [`Severity::Help`].
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::help(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), Some(Severity::Help));
    /// ```
    pub fn help(range: impl Into<SpanRange>, label: impl Into<String>) -> Self {
        Self {
            source: None,
            range: range.into(),
            message: label.into(),
            severity: Some(Severity::Help),
        }
    }

    /// Sets the source file on an existing label instance, `self`, and returns
    /// it.
    pub fn with_source(mut self, source: Option<Arc<dyn Source>>) -> Self {
        self.source = source;
        self
    }

    /// Gets the message of the current label instance.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// ```
    pub fn message(&self) -> &str {
        &self.message
    }

    /// Gets the integer span of the current label instance.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity, SpanRange};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.range(), &SpanRange(60..65));
    /// ```
    pub fn range(&self) -> &SpanRange {
        &self.range
    }

    /// Gets the source code of the current label instance.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity, Source, SpanRange};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()));
    ///
    /// assert_eq!(label.source().unwrap().name(), source.name());
    /// assert_eq!(label.source().unwrap().content(), source.content());
    /// ```
    pub fn source(&self) -> Option<Arc<dyn Source>> {
        self.source.clone()
    }

    /// Gets the severity of the current label instance.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()))
    ///     .with_severity(Severity::Warning);
    ///
    /// assert_eq!(label.severity(), Some(Severity::Warning));
    /// ```
    pub fn severity(&self) -> Option<Severity> {
        self.severity
    }

    /// Sets the severity for the current label instance.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(60..65, "could not find method 'invok'")
    ///     .with_source(Some(source.clone()))
    ///     .with_severity(Severity::Warning);
    ///
    /// assert_eq!(label.message(), "could not find method 'invok'");
    /// assert_eq!(label.severity(), Some(Severity::Warning));
    /// ```
    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = Some(severity);
        self
    }

    /// Reads a span of the source using the range within the
    /// label itself, including a dynamic amount of context lines.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Label, Severity};
    ///
    /// let source = Arc::new(r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return 0;
    /// }"#);
    ///
    /// let label = Label::new(58..67, String::new())
    ///     .with_source(Some(source.clone()));
    /// let span = label.read_span(None, 0).unwrap();
    ///
    /// assert_eq!(span.data, "    let b = a.invok();");
    /// assert_eq!(span.start_line, 2);
    /// assert_eq!(span.line, 2);
    /// ```
    pub fn read_span(&self, diagnostic: Option<&dyn Diagnostic>, context_lines: usize) -> Option<LabelSpan> {
        let diag_source = diagnostic.and_then(|d| d.source_code());
        let source = self.source.clone().or(diag_source)?;

        let content = source.content();
        let range = self.range().0.clone();

        let mut line_start = 0;
        let mut line_spans = Vec::new();

        for line in content.lines() {
            let line_len = line.len();
            let span = line_start..(line_start + line_len);

            line_spans.push(span);

            // +1 for '\n' (assuming UNIX-style newlines)
            line_start += line_len + 1;
        }

        // Determine the lines that intersect with the byte range
        let mut matching_lines = Vec::new();
        for (i, span) in line_spans.iter().enumerate() {
            if span.end > range.start && span.start < range.end {
                matching_lines.push(i);
            }
        }

        // If the range is outside the span of the input string,
        // we return the first context window of the string as a fallback.
        if matching_lines.is_empty() {
            // Get the end of the context window, if possible.
            // Otherwise, just return the entire string.
            let last_line_span = line_spans.get(context_lines * 2 + 1).or_else(|| line_spans.last());

            let last_line_idx = last_line_span.map(|s| s.end).unwrap_or_default();

            return Some(LabelSpan {
                data: content[0..last_line_idx].to_string(),
                start_line: context_lines,
                line: 0,
            });
        }

        let first_matching_line = *matching_lines.first().unwrap();

        let first_match = first_matching_line.saturating_sub(context_lines);
        let last_match = (matching_lines.last().unwrap() + context_lines).min(line_spans.len() - 1);

        let start_byte = line_spans[first_match].start;
        let end_byte = line_spans[last_match].end;

        Some(LabelSpan {
            data: content[start_byte..end_byte].to_string(),
            start_line: first_matching_line,
            line: first_match,
        })
    }
}

/// Represents a span within a label.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LabelSpan {
    /// Defines the string inside the associated span.
    pub data: String,

    /// Defines the zero-indexed line in the associated source, where the span
    /// starts (including context lines).
    pub line: usize,

    /// Defines the zero-indexed line in the associated source, where the span
    /// starts (excluding context lines).
    pub start_line: usize,
}

impl LabelSpan {
    /// Gets the line count in the span.
    pub fn line_count(&self) -> usize {
        self.data.lines().count()
    }
}

/// Represents a suggested fix with a source file attached.
///
/// Suggestions can guide the user to change some part of the source code,
/// in order to fix diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Suggestion {
    /// Defines some span within a file should be deleted.
    Deletion { range: SourceRange },

    /// Defines some string should be inserted at some position within a file.
    Insertion { location: SourceLocation, value: String },

    /// Defines some span within a file should be replaced.
    Replacement { range: SourceRange, replacement: String },
}

impl Suggestion {
    /// Creates a new [`Suggestion`] where a certain span within
    /// a file should be deleted.
    pub fn delete(range: SourceRange) -> Self {
        Self::Deletion { range }
    }

    /// Creates a new [`Suggestion`] where a certain location
    /// should have a string value inserted.
    pub fn insert(location: SourceLocation, value: impl Into<String>) -> Self {
        Self::Insertion {
            location,
            value: value.into(),
        }
    }

    /// Creates a new [`Suggestion`] where a certain span should
    /// be replaced with a string value.
    pub fn replace(range: SourceRange, replacement: impl Into<String>) -> Self {
        Self::Replacement {
            range,
            replacement: replacement.into(),
        }
    }

    /// Gets the source file of the suggestion.
    pub fn source(&self) -> Arc<dyn Source> {
        match self {
            Suggestion::Insertion { location, .. } => location.source.clone(),
            Suggestion::Deletion { range, .. } | Suggestion::Replacement { range, .. } => range.source.clone(),
        }
    }

    /// Gets the span which the suggestion refers to.
    ///
    /// All suggestion types, except insertions, returns the inner span
    /// directly, where-as insertions will create a new span with a distance
    /// of 1.
    pub fn span(&self) -> Range<usize> {
        match self {
            Suggestion::Replacement { range, .. } | Suggestion::Deletion { range, .. } => range.span.0.clone(),
            Suggestion::Insertion { location, .. } => location.offset..location.offset + 1,
        }
    }
}

/// Represents a help message, which can be attached to diagnostics to aid
/// users.
///
/// Each help message is accompanied by zero-or-more suggestions, which can
/// guide the user to change some part of the source code, in order to fix the
/// diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Help {
    /// Defines the actual message to print in the footer.
    pub message: String,

    /// A list of zero-or-more suggestions to apply to the original source code.
    pub suggestions: Vec<Suggestion>,
}

impl Help {
    /// Creates a new [`Help`] with the given message.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::Help;
    ///
    /// let help = Help::new("have you checked your syntax?");
    ///
    /// assert_eq!(help.message, "have you checked your syntax?");
    /// assert_eq!(help.suggestions, vec![]);
    /// ```
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            suggestions: Vec::new(),
        }
    }

    /// Adds the given suggestion to the help message.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Help, NamedSource, SourceRange, Suggestion};
    ///
    /// let source = Arc::new(NamedSource::new(
    ///     "src/lib.rs",
    ///     r#"fn main() -> int {
    ///     /// doc comment
    ///     let a = Testing::new();
    ///     let b = a.invoke();
    ///
    ///     return 0;
    /// }"#,
    /// ));
    ///
    /// let source_range = SourceRange::new(source.clone(), 23..38);
    /// let suggestion = Suggestion::delete(source_range);
    ///
    /// let help = Help::new("remove this doc comment")
    ///     .with_suggestion(suggestion.clone());
    ///
    /// assert_eq!(help.suggestions, vec![suggestion]);
    /// ```
    pub fn with_suggestion(mut self, suggestion: impl Into<Suggestion>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    /// Adds the given suggestions to the help message.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Help, NamedSource, SourceRange, Suggestion};
    ///
    /// let source = Arc::new(NamedSource::new(
    ///     "src/lib.rs",
    ///     r#"fn main() -> int {
    ///     let a = Testing::new();
    ///     let b = a.invoke();
    ///
    ///     return (0);
    /// }"#,
    /// ));
    ///
    /// let suggestion1 = Suggestion::delete(SourceRange::new(source.clone(), 83..84));
    /// let suggestion2 = Suggestion::delete(SourceRange::new(source.clone(), 85..86));
    ///
    /// let help = Help::new("remove this doc comment")
    ///     .with_suggestions([suggestion1.clone(), suggestion2.clone()]);
    ///
    /// assert_eq!(help.suggestions, vec![suggestion1, suggestion2]);
    /// ```
    pub fn with_suggestions(mut self, suggestions: impl IntoIterator<Item = Suggestion>) -> Self {
        self.suggestions.extend(suggestions);
        self
    }
}

impl From<&str> for Help {
    fn from(value: &str) -> Self {
        Help::new(value)
    }
}

impl From<String> for Help {
    fn from(value: String) -> Self {
        Help::new(value)
    }
}

impl From<&String> for Help {
    fn from(value: &String) -> Self {
        Help::new(value)
    }
}

/// Represents a single diagnostic message, which can be
/// pretty-printed into an intuitive and fancy error message.
pub trait Diagnostic: std::fmt::Debug {
    /// Defines which message to be raised to the user, when reported.
    fn message(&self) -> String;

    /// Diagnostic severity level.
    ///
    /// This may be used by the renderer to determine how to display the
    /// diagnostic or even halt the program, depending on the severity
    /// level.
    fn severity(&self) -> Severity {
        Severity::default()
    }

    /// Unique diagnostic code, which can be used to look up more information
    /// about the error.
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        None
    }

    /// Gets the source code which the diagnostic refers to.
    ///
    /// This isn't used if only defined by itself. It will only be used if one
    /// or more labels are defined without any source directly attached.
    fn source_code(&self) -> Option<Arc<dyn Source>> {
        None
    }

    /// Labels to attach to snippets of the source code.
    fn labels(&self) -> Option<Box<dyn Iterator<Item = Label> + '_>> {
        None
    }

    /// Any errors which were the underlying cause for the diagnostic to be
    /// raised.
    fn causes(&self) -> Box<dyn Iterator<Item = &(dyn Diagnostic + Send + Sync)> + '_> {
        Box::new(std::iter::empty())
    }

    /// Any related errors, which can be used to provide additional information
    /// about the diagnostic.
    fn related(&self) -> Box<dyn Iterator<Item = &(dyn Diagnostic + Send + Sync)> + '_> {
        Box::new(std::iter::empty())
    }

    /// Help messages, which can be used to provide additional information about
    /// the diagnostic.
    fn help(&self) -> Option<Box<dyn Iterator<Item = Help> + '_>> {
        None
    }
}

impl std::fmt::Display for Box<dyn Diagnostic + Send + Sync + 'static> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
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
    fn from(err: Box<dyn std::error::Error + Send + Sync>) -> Self {
        err.into_diagnostic()
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

/// Trait for converting implementations into implementations of [`Diagnostic`].
pub trait IntoDiagnostic {
    /// Converts the instance into an implementation of [`Diagnostic`].
    fn into_diagnostic(self) -> Box<dyn Diagnostic + Send + Sync>;
}

impl<T: std::error::Error + Send + Sync> IntoDiagnostic for T {
    fn into_diagnostic(self) -> Box<dyn Diagnostic + Send + Sync> {
        Box::new(SimpleDiagnostic::new(self.to_string()))
    }
}

/// Diagnostic which can be created at runtime.
#[derive(Default, Debug)]
pub struct SimpleDiagnostic {
    /// Defines the message being displayed along with the diagnostic.
    pub message: String,

    /// Unique code for the diagnostic, which can be used to look up
    /// more information about the diagnostic.
    pub code: Option<String>,

    /// Defines the severity of the diagnostic. Defaults to `Severity::Error`.
    pub severity: Severity,

    /// Defines a list of help messages which can help or guide the user about
    /// the diagnostic.
    pub help: Vec<Help>,

    /// Defines a list of labels which can provide additional context about the
    /// diagnostic.
    pub labels: Option<Vec<Label>>,

    /// Defines the underlying cause for the diagnostic to be raised.
    pub causes: Vec<Box<dyn Diagnostic + Send + Sync>>,

    /// Defines the diagnostics which are related to the current one, if any.
    pub related: Vec<Box<dyn Diagnostic + Send + Sync>>,
}

impl SimpleDiagnostic {
    /// Creates a new [`SimpleDiagnostic`] with the given message content.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::SimpleDiagnostic;
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.to_string(), "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.message, "Whoops, that wasn't supposed to happen!");
    /// ```
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            ..Self::default()
        }
    }

    /// Sets the severity for the current diagnostic instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::{Severity, SimpleDiagnostic};
    ///
    /// let diag = SimpleDiagnostic::new("Hmm, this could certainly be done better.")
    ///     .with_severity(Severity::Warning);
    ///
    /// assert_eq!(diag.message, "Hmm, this could certainly be done better.");
    /// assert_eq!(diag.severity, Severity::Warning);
    /// ```
    pub fn with_severity(mut self, severity: impl Into<Severity>) -> Self {
        self.severity = severity.into();
        self
    }

    /// Sets the diagnostic code for the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::SimpleDiagnostic;
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!")
    ///     .with_code("E1010");
    ///
    /// assert_eq!(diag.message, "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.code, Some(String::from("E1010")));
    /// ```
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Adds a new help message to the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::{Help, SimpleDiagnostic};
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!")
    ///     .with_help("have you tried restarting?");
    ///
    /// assert_eq!(diag.message, "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.help, vec![Help::new("have you tried restarting?")]);
    /// ```
    pub fn with_help(mut self, help: impl Into<Help>) -> Self {
        self.help.push(help.into());
        self
    }

    /// Sets the help message of the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::{Help, SimpleDiagnostic};
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!")
    ///     .set_help("have you tried restarting?");
    ///
    /// assert_eq!(diag.message, "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.help, vec![Help::new("have you tried restarting?")]);
    /// ```
    pub fn set_help(mut self, help: impl Into<Help>) -> Self {
        self.help = vec![help.into()];
        self
    }

    /// Adds a new label to the current instance.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{SimpleDiagnostic, Label, NamedSource};
    ///
    /// let source = Arc::new(NamedSource::new(
    ///     "src/lib.rs",
    ///     r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return false;
    /// }"#,
    /// ));
    ///
    /// let label1 = Label::new(60..65, "could not find method 'invok'").with_source(Some(source.clone()));
    /// let label2 = Label::new(81..86, "expected 'int', found 'boolean'").with_source(Some(source.clone()));
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!")
    ///     .with_label(label1.clone())
    ///     .with_label(label2.clone());
    ///
    /// assert_eq!(diag.message, "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.labels, Some(vec![label1, label2]));
    /// ```
    pub fn with_label(mut self, label: impl Into<Label>) -> Self {
        let mut labels = self.labels.unwrap_or_default();
        labels.push(label.into());

        self.labels = Some(labels);
        self
    }

    /// Adds a list of labels to the current instance. The given
    /// labels are appended onto the existing label array in the
    /// diagnostic, so nothing is overwritten.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{SimpleDiagnostic, Label, NamedSource};
    ///
    /// let source = Arc::new(NamedSource::new(
    ///     "src/lib.rs",
    ///     r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return false;
    /// }"#,
    /// ));
    ///
    /// let label1 = Label::new(60..65, "could not find method 'invok'").with_source(Some(source.clone()));
    /// let label2 = Label::new(81..86, "expected 'int', found 'boolean'").with_source(Some(source.clone()));
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!")
    ///     .with_labels([label1.clone(), label2.clone()]);
    ///
    /// assert_eq!(diag.message, "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.labels, Some(vec![label1, label2]));
    /// ```
    pub fn with_labels(mut self, labels: impl IntoIterator<Item = impl Into<Label>>) -> Self {
        let labels = labels
            .into_iter()
            .map(|r| Into::<Label>::into(r))
            .collect::<Vec<Label>>();

        let mut all_labels = self.labels.unwrap_or_default();
        all_labels.extend(labels);

        self.labels = Some(all_labels);
        self
    }

    /// Adds a related diagnostic to the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::SimpleDiagnostic;
    ///
    /// let related1 = std::io::Error::new(std::io::ErrorKind::Other, "failed to read file");
    /// let related2 = std::io::Error::new(std::io::ErrorKind::Other, "file is unaccessible");
    ///
    /// let diag = SimpleDiagnostic::new("failed to perform I/O operation")
    ///     .add_related(related1)
    ///     .add_related(related2);
    ///
    /// assert_eq!(diag.message, "failed to perform I/O operation");
    /// assert_eq!(diag.related.iter().map(|e| e.to_string()).collect::<Vec<_>>(), vec![
    ///     "failed to read file".to_string(),
    ///     "file is unaccessible".to_string()
    /// ]);
    /// ```
    pub fn add_related(mut self, related: impl Into<Box<dyn Diagnostic + Send + Sync>>) -> Self {
        self.related.push(related.into());
        self
    }

    /// Adds multiple related diagnostics to the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::SimpleDiagnostic;
    ///
    /// let related1 = std::io::Error::new(std::io::ErrorKind::Other, "failed to read file");
    /// let related2 = std::io::Error::new(std::io::ErrorKind::Other, "file is unaccessible");
    ///
    /// let diag = SimpleDiagnostic::new("failed to perform I/O operation")
    ///     .append_related([related1, related2]);
    ///
    /// assert_eq!(diag.message, "failed to perform I/O operation");
    /// assert_eq!(diag.related.iter().map(|e| e.to_string()).collect::<Vec<_>>(), vec![
    ///     "failed to read file".to_string(),
    ///     "file is unaccessible".to_string()
    /// ]);
    /// ```
    pub fn append_related(
        mut self,
        related: impl IntoIterator<Item = impl Into<Box<dyn Diagnostic + Send + Sync>>>,
    ) -> Self {
        let related = related
            .into_iter()
            .map(|r| Into::<Box<dyn Diagnostic + Send + Sync>>::into(r))
            .collect::<Vec<Box<dyn Diagnostic + Send + Sync>>>();

        self.related.extend(related);
        self
    }

    /// Adds a causing error diagnostic to the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::SimpleDiagnostic;
    ///
    /// let cause1 = std::io::Error::new(std::io::ErrorKind::Other, "failed to read file");
    /// let cause2 = std::io::Error::new(std::io::ErrorKind::Other, "file is unaccessible");
    ///
    /// let diag = SimpleDiagnostic::new("failed to perform I/O operation")
    ///     .add_cause(cause1)
    ///     .add_cause(cause2);
    ///
    /// assert_eq!(diag.message, "failed to perform I/O operation");
    /// assert_eq!(diag.causes.iter().map(|e| e.to_string()).collect::<Vec<_>>(), vec![
    ///     "failed to read file".to_string(),
    ///     "file is unaccessible".to_string()
    /// ]);
    /// ```
    pub fn add_cause(mut self, cause: impl Into<Box<dyn Diagnostic + Send + Sync>>) -> Self {
        self.causes.push(cause.into());
        self
    }

    /// Adds multiple causing error diagnostics to the current instance.
    ///
    /// # Examples
    /// ```
    /// use lume_errors::SimpleDiagnostic;
    ///
    /// let cause1 = std::io::Error::new(std::io::ErrorKind::Other, "failed to read file");
    /// let cause2 = std::io::Error::new(std::io::ErrorKind::Other, "file is unaccessible");
    ///
    /// let diag = SimpleDiagnostic::new("failed to perform I/O operation")
    ///     .add_causes([cause1, cause2]);
    ///
    /// assert_eq!(diag.message, "failed to perform I/O operation");
    /// assert_eq!(diag.causes.iter().map(|e| e.to_string()).collect::<Vec<_>>(), vec![
    ///     "failed to read file".to_string(),
    ///     "file is unaccessible".to_string()
    /// ]);
    /// ```
    pub fn add_causes(
        mut self,
        causes: impl IntoIterator<Item = impl Into<Box<dyn Diagnostic + Send + Sync>>>,
    ) -> Self {
        let causes = causes
            .into_iter()
            .map(|r| Into::<Box<dyn Diagnostic + Send + Sync>>::into(r))
            .collect::<Vec<Box<dyn Diagnostic + Send + Sync>>>();

        self.causes.extend(causes);
        self
    }
}

impl Diagnostic for SimpleDiagnostic {
    fn message(&self) -> String {
        self.message.clone()
    }

    fn severity(&self) -> Severity {
        self.severity
    }

    fn code(&self) -> Option<Box<dyn Display + '_>> {
        self.code.as_ref().map(|c| Box::new(c) as Box<dyn Display>)
    }

    fn help(&self) -> Option<Box<dyn Iterator<Item = Help> + '_>> {
        Some(Box::new(self.help.clone().into_iter()))
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = Label> + '_>> {
        self.labels
            .as_ref()
            .map(|ls| ls.iter().cloned())
            .map(Box::new)
            .map(|b| b as Box<dyn Iterator<Item = Label>>)
    }

    fn related(&self) -> Box<dyn Iterator<Item = &(dyn Diagnostic + Send + Sync)> + '_> {
        Box::new(self.related.iter().map(|b| b.as_ref()))
    }

    fn causes(&self) -> Box<dyn Iterator<Item = &(dyn Diagnostic + Send + Sync)> + '_> {
        Box::new(self.causes.iter().map(|b| b.as_ref()))
    }
}

impl std::fmt::Display for SimpleDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug)]
pub struct SourceWrapped {
    pub(crate) diagnostic: Box<dyn Diagnostic + Send + Sync>,
    pub(crate) source: Arc<dyn Source + Send + Sync>,
}

impl Diagnostic for SourceWrapped {
    fn message(&self) -> String {
        self.diagnostic.message()
    }

    fn severity(&self) -> Severity {
        self.diagnostic.severity()
    }

    fn code(&self) -> Option<Box<dyn Display + '_>> {
        self.diagnostic.code()
    }

    fn help(&self) -> Option<Box<dyn Iterator<Item = Help> + '_>> {
        self.diagnostic.help()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = Label> + '_>> {
        self.diagnostic.labels()
    }

    fn related(&self) -> Box<dyn Iterator<Item = &(dyn Diagnostic + Send + Sync)> + '_> {
        self.diagnostic.related()
    }

    fn causes(&self) -> Box<dyn Iterator<Item = &(dyn Diagnostic + Send + Sync)> + '_> {
        self.diagnostic.causes()
    }

    fn source_code(&self) -> Option<Arc<dyn Source>> {
        self.diagnostic.source_code().or_else(|| Some(self.source.clone()))
    }
}

impl std::fmt::Display for SourceWrapped {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

pub trait WithSource {
    /// Provides the current diagnostic with source code, so it
    /// can still be reported, even though no source is available at
    /// the time of diagnostic creation.
    ///
    /// # Examples
    /// ```
    /// use std::sync::Arc;
    /// use lume_errors::{Diagnostic, SimpleDiagnostic, Label, NamedSource, Source, WithSource};
    ///
    /// // no source attached
    /// let label = Label::new(60..65, "could not find method 'invok'");
    ///
    /// let diag = SimpleDiagnostic::new("Whoops, that wasn't supposed to happen!")
    ///     .with_label(label.clone());
    ///
    /// let source = Arc::new(NamedSource::new(
    ///     "src/lib.rs",
    ///     r#"fn main() -> int {
    ///     let a = new Testing();
    ///     let b = a.invok();
    ///
    ///     return false;
    /// }"#,
    /// ));
    ///
    /// // attach the source code to the diagnostic
    /// let diag = diag.with_source(source.clone());
    ///
    /// assert_eq!(diag.message(), "Whoops, that wasn't supposed to happen!");
    /// assert_eq!(diag.source_code().unwrap().name(), source.name());
    /// assert_eq!(diag.source_code().unwrap().content(), source.content());
    /// ```
    fn with_source(self, source: Arc<dyn Source>) -> impl Diagnostic;
}

impl<T: Diagnostic + Send + Sync + 'static> WithSource for T {
    fn with_source(self, source: Arc<dyn Source>) -> impl Diagnostic {
        SourceWrapped {
            diagnostic: Box::new(self),
            source,
        }
    }
}
