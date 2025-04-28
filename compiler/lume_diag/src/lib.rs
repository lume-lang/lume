use crate::source::*;
use ariadne::{ColorGenerator, Report, ReportKind, sources};
use indexmap::IndexMap;
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

impl From<Severity> for ariadne::ReportKind<'_> {
    fn from(val: Severity) -> ariadne::ReportKind<'static> {
        match val {
            Severity::Error => ariadne::ReportKind::Custom("error", ariadne::Color::Red),
            Severity::Warning => ariadne::ReportKind::Custom("warning", ariadne::Color::Yellow),
            Severity::Info => ariadne::ReportKind::Custom("info", ariadne::Color::BrightBlue),
            Severity::Note => ariadne::ReportKind::Custom("note", ariadne::Color::BrightBlue),
            Severity::Help => ariadne::ReportKind::Custom("help", ariadne::Color::BrightBlue),
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
    pub fn new(source: &'a dyn source::Source, range: impl Into<SpanRange>, label: String) -> Self {
        Self {
            source,
            range: range.into(),
            label,
        }
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

    /// Any related errors, which can be used to provide additional information about the diagnostic.
    fn related<'a>(&'a self) -> Vec<LumeDiagnostic<'a>> {
        Vec::new()
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
            related: self.related(),
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

    /// Zero-or-more related diagnostics, which can point to related locations in the source code.
    pub related: Vec<LumeDiagnostic<'a>>,

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
    ///     related: Vec::new(),
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
            related: Vec::new(),
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

    /// Adds the given diagnostic to the diagnostic.
    pub fn add_related(mut self, diagnostic: LumeDiagnostic<'a>) -> Self {
        self.related.push(diagnostic);

        self
    }

    /// Adds the given related diagnostics to the diagnostic.
    pub fn append_related(mut self, diagnostics: impl IntoIterator<Item = LumeDiagnostic<'a>>) -> Self {
        self.related.extend(diagnostics);

        self
    }

    /// Renders the diagnostic message to the console.
    pub fn render(self) {
        let level: ReportKind = self.severity.into();

        let mut builder = Report::build(level, (String::new(), 0..0));
        let mut colors = ColorGenerator::from_state([25000, 5000, 28000], 0.7);
        let mut source_map: IndexMap<String, &str> = IndexMap::new();

        builder = builder.with_message(self.message);

        if let Some(code) = self.code {
            builder = builder.with_code(code);
        }

        if let Some(labels) = self.labels {
            for label in labels {
                let source_name = label.source.name().unwrap_or("<unknown>");

                source_map.insert(source_name.to_string(), *label.source.content());

                let color = colors.next();
                let label = ariadne::Label::new((source_name.to_string(), label.range.0.clone()))
                    .with_message(&label.label)
                    .with_color(color);

                builder = builder.with_label(label);
            }
        }

        if let Some(help) = self.help {
            for line in help {
                builder = builder.with_help(line);
            }
        }

        builder.finish().print(sources(source_map)).unwrap();
    }
}

impl<'a> std::error::Error for LumeDiagnostic<'a> {}

impl<'a> std::fmt::Display for LumeDiagnostic<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
