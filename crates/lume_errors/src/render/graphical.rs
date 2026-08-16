use std::collections::HashSet;
use std::fmt::Display;
use std::ops::Range;
use std::sync::Arc;

use indexmap::IndexMap;
use owo_colors::{OwoColorize, Style, Styled};

use super::Formatter;
use crate::render::Renderer;
use crate::{Diagnostic, Help, Label, Severity, Source, SpanRange, Suggestion};

#[derive(Debug, Clone)]
pub struct ThemeStyle {
    pub error: Style,
    pub warning: Style,
    pub info: Style,
    pub note: Style,
    pub help: Style,

    pub deletion: Style,
    pub insertion: Style,

    pub link: Style,
    pub gutter: Style,
}

impl ThemeStyle {
    /// Defines a preset which utilizes RGB colors within the terminal.
    pub fn rgb() -> Self {
        ThemeStyle {
            error: Style::new().fg_rgb::<233, 114, 99>().bold(),
            warning: Style::new().fg_rgb::<235, 191, 131>().bold(),
            info: Style::new().fg_rgb::<114, 159, 207>(),
            note: Style::new().fg_rgb::<166, 227, 161>(),
            help: Style::new().fg_rgb::<171, 161, 247>(),

            deletion: Style::new().fg_rgb::<233, 114, 99>(),
            insertion: Style::new().fg_rgb::<166, 227, 161>(),

            link: Style::new().fg_rgb::<166, 173, 200>(),
            gutter: Style::new().fg_rgb::<156, 156, 192>(),
        }
    }

    /// Defines a preset which utilizes ANSI color codes within the terminal.
    pub fn ansi() -> Self {
        ThemeStyle {
            error: Style::new().bright_red().bold(),
            warning: Style::new().bright_yellow().bold(),
            info: Style::new().bright_blue().bold(),
            note: Style::new().bright_green().bold(),
            help: Style::new().bright_cyan().bold(),

            deletion: Style::new().bright_red(),
            insertion: Style::new().bright_green(),

            link: Style::new().bright_white(),
            gutter: Style::new().bright_white(),
        }
    }

    /// Retrieves the style which is utilized for the given severity.
    pub fn from_severity(&self, severity: Severity) -> Style {
        match severity {
            Severity::Error => self.error,
            Severity::Warning => self.warning,
            Severity::Info => self.info,
            Severity::Note => self.note,
            Severity::Help => self.help,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ThemeSymbols {
    pub error: &'static str,
    pub warning: &'static str,
    pub info: &'static str,
    pub note: &'static str,
    pub help: &'static str,
}

impl ThemeSymbols {
    pub fn unicode() -> Self {
        ThemeSymbols {
            error: "×",
            warning: "⚠",
            info: "☞",
            note: "☞",
            help: "☞",
        }
    }

    pub fn from_severity(&self, severity: Severity) -> &'static str {
        match severity {
            Severity::Error => self.error,
            Severity::Warning => self.warning,
            Severity::Info => self.info,
            Severity::Note => self.note,
            Severity::Help => self.help,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrowSymbols {
    /// "─"
    pub hbar: char,

    /// "┬"
    pub hbot: char,

    /// "│"
    pub vertical: char,

    /// "∶"
    pub vertical_break: char,

    /// "╭"
    pub top_left: char,

    /// "╰"
    pub bottom_left: char,

    /// "├"
    pub horizontal_right: char,

    /// "^"
    pub arrow_up: char,

    /// ">"
    pub arrow_right: char,
}

impl ArrowSymbols {
    pub fn unicode() -> Self {
        ArrowSymbols {
            hbar: '─',
            hbot: '┬',
            vertical: '│',
            vertical_break: '∶',
            top_left: '╭',
            bottom_left: '╰',
            horizontal_right: '├',
            arrow_up: '^',
            arrow_right: '▶',
        }
    }
}

#[derive(Debug, Clone)]
pub struct Theme {
    pub style: ThemeStyle,
    pub symbols: ThemeSymbols,
    pub arrows: ArrowSymbols,
}

impl Theme {
    /// Returns an instance of [`Theme`] which uses the "fancy" preset.
    ///
    /// The fancy preset uses RGB colors and unicode symbols for the
    /// diagnostics.
    pub fn fancy() -> Self {
        Theme {
            style: ThemeStyle::rgb(),
            symbols: ThemeSymbols::unicode(),
            arrows: ArrowSymbols::unicode(),
        }
    }
}

/// An implementation of [`Renderer`] which displays diagnostics in a graphical
/// way in the console using colors, Unicode symbols and highlighting.
///
/// # Examples
///
/// ```
/// use lume_errors::{Renderer, GraphicalRenderer};
///
/// let renderer = GraphicalRenderer::new();
/// ```
#[derive(Debug, Clone)]
pub struct GraphicalRenderer {
    /// Defines the theme of the renderer.
    ///
    /// The theme defines which colors and symbols to use when rendering
    /// diagnostics.
    pub theme: Theme,

    /// Defines the padding to use per level of identation.
    pub padding: usize,

    /// Defines the margin to use in the gutter of snippets.
    pub gutter_margin: usize,

    /// Defines the amount of lines surrounding a label to include as context.
    pub context_lines: usize,

    /// Defines whether to use colors in the output.
    pub use_colors: bool,

    /// Defines whether to highlight the source code where a label
    /// is marked. This is only used if `use_colors` is `true`.
    pub highlight_source: bool,

    /// Defiens the current indentation level.
    current_indent: usize,
}

impl Default for GraphicalRenderer {
    fn default() -> Self {
        GraphicalRenderer::new()
    }
}

impl Renderer for GraphicalRenderer {
    fn render_fmt(&mut self, f: &mut Formatter<'_>, diagnostic: &dyn Diagnostic) -> std::fmt::Result {
        self.render_diagnostic(f, diagnostic)
    }
}

impl GraphicalRenderer {
    /// Creates a new instance of [`GraphicalRenderer`] with default settings.
    pub fn new() -> Self {
        GraphicalRenderer {
            theme: Theme::fancy(),
            padding: 6,
            gutter_margin: 2,
            context_lines: 1,
            use_colors: true,
            highlight_source: false,
            current_indent: 0,
        }
    }

    fn severity_style(&self, severity: Severity) -> Style {
        if self.use_colors {
            self.theme.style.from_severity(severity)
        } else {
            Style::new()
        }
    }

    /// Gets the current indentation to use, in amounts of spaces.
    fn ident(&self) -> usize {
        self.current_indent * self.padding
    }

    /// Writes the current indentation to the given writer.
    fn write_ident(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        write_padding(f, self.ident())
    }

    /// Styles the given value with the provided style.
    ///
    /// If colors are disabled on the renderer, no styles are applied and the
    /// value is kept unstyled.
    fn style<'a, T: std::fmt::Display>(&self, val: &'a T, style: Style) -> Styled<&'a T> {
        if self.use_colors {
            val.style(style)
        } else {
            val.style(Style::new())
        }
    }

    /// Determines how much padding to use for the gutter of the
    /// given source code. The gutter margin is included in the result.
    fn gutter_size_of(&self, source: &str) -> usize {
        let largest_line_size = source.lines().count().to_string().len();

        largest_line_size + self.gutter_margin
    }

    /// Renders the given diagnostic to the provided writer.
    ///
    /// # Example
    ///
    /// ```text
    /// error[E4012]
    ///    × invalid doc comment found
    ///     ╭─[std/array.lm:32:8]
    ///  31 │
    ///  32 │        /// Allocate the minimum amount of capacity in the array.
    ///  33 │        array.reserve(capacity);
    ///     │        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doc comment found on statement
    ///     ╰──
    ///    help: doc comments are only allowed on definitions
    /// ```
    fn render_diagnostic(&mut self, f: &mut impl std::fmt::Write, diagnostic: &dyn Diagnostic) -> std::fmt::Result {
        owo_colors::with_override(self.use_colors, || {
            self.render_header(f, diagnostic)?;
            self.render_source(f, diagnostic)?;
            self.render_footer(f, diagnostic)?;

            Result::Ok(())
        })
    }

    /// Renders the header of the diagnostic message, which includes severity
    /// and diagnostic code (if any).
    ///
    /// # Example
    ///
    /// ```text
    ///   × error[E4012]: invalid doc comment found
    /// ```
    fn render_header(&self, f: &mut impl std::fmt::Write, diagnostic: &dyn Diagnostic) -> std::fmt::Result {
        let severity_symbol = self.theme.symbols.from_severity(diagnostic.severity());
        let severity_style = self.severity_style(diagnostic.severity());
        let severity_str = diagnostic.severity().to_string();

        self.write_ident(f)?;
        write!(
            f,
            "{} {}",
            self.style(&severity_symbol, severity_style),
            self.style(&severity_str, severity_style)
        )?;

        if let Some(code) = &diagnostic.code() {
            write!(f, "{}", self.style(&format!("[{code}]"), severity_style))?;
        }

        writeln!(f, ": {}", diagnostic.message())
    }

    /// Renders the source span of the diagnostic, if any, attached with any
    /// associated labels.
    ///
    /// # Example
    ///
    /// ```text
    ///       ╭─[std/array.lm:29:46]
    ///    28 │    /// When creating an array with a set capacity, it's length will still be zero.
    ///    29 │    pub fn with_capacity(capacity: UInt64) -> Array<T> {
    ///       │                                              ^^^^^^^^ expected type `Array<T>` found here
    ///       ·
    ///    34 │
    ///    35 │        return true;
    ///       │        ^^^^^^^^^^^^ expected `Array<T>`, found `Boolean`
    ///       ╰──
    /// ```
    fn render_source(&mut self, f: &mut impl std::fmt::Write, diagnostic: &dyn Diagnostic) -> std::fmt::Result {
        for cause in diagnostic.causes() {
            self.current_indent += 1;

            self.render_diagnostic(f, cause)?;
            writeln!(f)?;

            self.current_indent -= 1;
        }

        if let Some(labels) = diagnostic.labels() {
            let mut label_groups: IndexMap<Option<String>, LabelGroup> = IndexMap::new();

            // Group the labels into groups where all elements have
            // the same source file. This helps prevent multiple label
            // headers in a row from defining the same file path.
            for label in labels {
                // If no source code is attached to the label itself, see if
                // a source is attached to the parent diagnostic.
                //
                // If no source is found on either, skip over the label entirely.
                //
                // TODO: should be print a warning when no source is found?
                let source = match label.source() {
                    Some(s) => s.clone(),
                    None => match diagnostic.source_code() {
                        Some(s) => s,
                        None => continue,
                    },
                };

                let source_name = source.name().map(|n| n.to_string());

                label_groups
                    .entry(source_name)
                    .or_insert(LabelGroup {
                        labels: Vec::new(),
                        source,
                    })
                    .labels
                    .push(label);
            }

            for (_, group) in label_groups {
                self.render_label_group(f, group, diagnostic.severity())?;
            }
        }

        for related in diagnostic.related() {
            self.current_indent += 1;

            self.render_diagnostic(f, related)?;
            writeln!(f)?;

            self.current_indent -= 1;
        }

        Ok(())
    }

    /// Renders a label group context with one-or-more labels, all sharing the
    /// same source file.
    ///
    /// # Example
    ///
    /// ```text
    ///    28 │    /// When creating an array with a set capacity, it's length will still be zero.
    ///    29 │    pub fn with_capacity(capacity: UInt64) -> Array<T> {
    ///       │                                              ^^^^^^^^ expected type `Array<T>` found here
    ///       ·
    ///    34 │
    ///    35 │        return true;
    ///       │        ^^^^^^^^^^^^ expected `Array<T>`, found `Boolean`
    /// ```
    fn render_label_context(
        &self,
        f: &mut impl std::fmt::Write,
        context: LabelContext,
        severity: Severity,
    ) -> std::fmt::Result {
        let source_content = context.source.content();
        let gutter_size = self.gutter_size_of(&source_content);

        let joined_span = context.max_span();
        let span = coords_of_span(&source_content, joined_span.clone());

        let style = self.severity_style(severity);
        let arrows = &self.theme.arrows;

        // Render all the labels in in the group, along with joiners in the vertical
        // gutter.
        //
        //  28 │    /// When creating an array with a set capacity, it's length will
        // still be zero.  29 │    pub fn with_capacity(capacity: UInt64) ->
        // Array<T> {     │
        // ^^^^^^^^ expected type `Array<T>` found here     ·
        //  34 │
        //  35 │        return true;
        //     │        ^^^^^^^^^^^^ expected `Array<T>`, found `Boolean`
        let content = extract_with_context(&source_content, joined_span.0, self.context_lines);

        let lines = content.lines().collect::<Vec<_>>();
        let line_count = lines.len();

        // Save all the coordinates of each label span, since we'll be needing them in
        // this function.
        let labels = context
            .children
            .iter()
            .map(|(_, l)| (l, coords_of_span(&source_content, l.range.0.clone())))
            .collect::<Vec<_>>();

        for (idx, line) in lines.into_iter().enumerate() {
            let line_num = span.start.line.saturating_sub(self.context_lines) + idx + 1;

            let mut line_labels = labels
                .iter()
                .filter(|(_, s)| !s.is_multiline() && s.start.line == span.start.line + idx)
                .collect::<Vec<_>>();

            line_labels.sort_by_key(|a| a.1.start.column);

            self.render_snippet_line_gutter(f, gutter_size, line_num)?;

            if span.is_multiline() {
                match idx {
                    0 => write!(
                        f,
                        "{}{}{} ",
                        arrows.top_left.style(style),
                        arrows.hbar.style(style),
                        arrows.arrow_right.style(style)
                    )?,
                    n if n == line_count - 1 => write!(
                        f,
                        "{}{}{} ",
                        arrows.horizontal_right.style(style),
                        arrows.hbar.style(style),
                        arrows.arrow_right.style(style)
                    )?,
                    _ => write!(f, "{}   ", arrows.vertical.style(style))?,
                }
            }

            if self.highlight_source {
                let mut style_line = StyledText::new(line.to_string());

                for (label, label_span) in &line_labels {
                    let severity = label.severity.unwrap_or(severity);
                    let style = self.severity_style(severity);

                    style_line.style_span(label_span.start.column..label_span.end.column, style);
                }

                // Style the labelled span correctly, if no child labels are directly
                // defined on the line itself.
                if !span.is_multiline() && line_num - 1 == span.start.line && line_labels.is_empty() {
                    let severity = context.parent.severity.unwrap_or(severity);
                    let style = self.severity_style(severity);

                    style_line.style_span(span.start.column..span.end.column, style);
                }

                writeln!(f, "{style_line}")?;
            } else {
                writeln!(f, "{line}")?;
            }

            if !span.is_multiline() && line_num - 1 == span.start.line && line_labels.is_empty() {
                self.render_line_labels(f, severity, vec![&(&context.parent, span)], gutter_size, false)?;
            } else {
                self.render_line_labels(f, severity, line_labels, gutter_size, true)?;
            }
        }

        if span.is_multiline() {
            self.render_snippet_break(f, gutter_size)?;
            writeln!(f, "{}", arrows.vertical.style(style))?;

            self.render_snippet_line_empty_gutter(f, gutter_size)?;
            writeln!(
                f,
                "{} {}",
                arrows.bottom_left.style(style),
                context.parent.message.style(style)
            )?;
        }

        Ok(())
    }

    /// Renders the labels under a given line, so each labelled span is
    /// underlined and directing the reader to the label message.
    ///
    /// # Example
    ///
    /// ```text
    ///   · │       ─┬    ┬
    ///   · │        │    ╰─ This has type of Str
    ///   · │        ╰─ This has type of Void
    /// ```
    fn render_line_labels(
        &self,
        f: &mut impl std::fmt::Write,
        severity: Severity,
        labels: Vec<&(&Label, Span)>,
        gutter_size: usize,
        is_multiline: bool,
    ) -> std::fmt::Result {
        if labels.is_empty() {
            return Ok(());
        }

        // If there is only a single label on the line, we can render it more compactly.
        let render_single_line = labels.len() == 1;

        let style = self.severity_style(severity);
        let arrows = &self.theme.arrows;

        // Write the underlines of each labelled span of the snippet.
        //
        //  2 │     () => 5,
        //    │     ─┬    ┬
        self.render_snippet_break(f, gutter_size)?;
        if is_multiline {
            write!(f, "{}   ", arrows.vertical.style(style))?;
        }

        let underline_len = labels.iter().map(|(_, s)| s.end.column).max().unwrap_or_default();
        let mut underline_str = StyledText::new(" ".repeat(underline_len));

        for (label, span) in &labels {
            let severity = label.severity.unwrap_or(severity);
            let style = self.severity_style(severity);

            for offset in span.columns() {
                let c = if render_single_line {
                    arrows.arrow_up
                } else if offset == span.columns().end - 1 {
                    arrows.hbot
                } else {
                    arrows.hbar
                };

                str_set_char(&mut underline_str.str, offset, c);
            }

            underline_str.style_span(span.columns(), style);

            if render_single_line {
                underline_str.append(&format!(" {}", label.message), style);
            }
        }

        if self.use_colors {
            writeln!(f, "{underline_str}")?;
        } else {
            writeln!(f, "{}", underline_str.str)?;
        }

        // After writing the underlines, we render the lines which go below it to
        // point to the message of each underline.
        //
        //    │        │    ╰── This is of type Nat
        //    │        ╰── This is of type Nil
        if !render_single_line {
            let mut label_text_lines = labels
                .iter()
                .map(|(_, span)| StyledText::new(" ".repeat(span.end.column + 1)))
                .collect::<Vec<_>>();

            for (idx, (label, span)) in labels.iter().enumerate() {
                let severity = label.severity.unwrap_or(severity);
                let style = self.severity_style(severity);

                let last_column = span.end.column.saturating_sub(1);

                #[allow(clippy::needless_range_loop, reason = "not looping entire collection")]
                for line_idx in 0..idx {
                    // Sets the vertical line in all preceding lines from the current one.
                    str_set_char(&mut label_text_lines[line_idx].str, last_column, arrows.vertical);

                    label_text_lines[line_idx].style_span(last_column..span.end.column, style);
                }

                let line = &mut label_text_lines[idx];

                str_set_char(&mut line.str, last_column, arrows.bottom_left);
                str_set_char(&mut line.str, span.end.column, arrows.hbar);
                str_set_char(&mut line.str, span.end.column + 1, arrows.hbar);

                line.style_span(last_column..span.end.column + 1, style);

                line.append(" ", style);
                line.append(&label.message, style);
            }

            for label_text_line in label_text_lines {
                self.render_snippet_break(f, gutter_size)?;

                if is_multiline {
                    write!(f, "{}   ", arrows.vertical.style(style))?;
                }

                if self.use_colors {
                    writeln!(f, "{label_text_line}")?;
                } else {
                    writeln!(f, "{}", label_text_line.str)?;
                }
            }
        }

        Ok(())
    }

    /// Renders a label group with one-or-more labels, all sharing the same
    /// source file.
    ///
    /// # Example
    ///
    /// ```text
    ///       ╭─[std/array.lm:29:46]
    ///    28 │    /// When creating an array with a set capacity, it's length will still be zero.
    ///    29 │    pub fn with_capacity(capacity: UInt64) -> Array<T> {
    ///       │                                              ^^^^^^^^ expected type `Array<T>` found here
    ///       ∶
    ///    34 │
    ///    35 │        return true;
    ///       │        ^^^^^^^^^^^^ expected `Array<T>`, found `Boolean`
    ///       ╰──
    /// ```
    fn render_label_group(
        &self,
        f: &mut impl std::fmt::Write,
        group: LabelGroup,
        severity: Severity,
    ) -> std::fmt::Result {
        if group.labels.is_empty() {
            return Ok(());
        }

        // We're assuming the first label is the "most important one", for no
        // reason in particular, but it seems the most intuitive.
        let first_label = group.labels.first().unwrap();

        let source = group.source;
        let source_name = source.name();
        let source_content = source.content();
        let gutter_size = self.gutter_size_of(&source_content);

        // Render header for the label group.
        //
        //    ╭─[std/array.lm:35:8]
        //
        let Span { start, .. } = coords_of_span(&source_content, first_label.range().clone());
        self.render_snippet_header(f, source_name, gutter_size, start.line, start.column)?;

        // Render all the labels in in the group, along with joiners in the vertical
        // gutter.
        //
        //  28 │    /// When creating an array with a set capacity, it's length will
        // still be zero.  29 │    pub fn with_capacity(capacity: UInt64) ->
        // Array<T> {     │
        // ^^^^^^^^ expected type `Array<T>` found here     ∶
        //  34 │
        //  35 │        return true;
        //     │        ^^^^^^^^^^^^ expected `Array<T>`, found `Boolean`
        let contexts = group_overlapping_labels(Some(source.clone()), group.labels.into_iter());
        let count = contexts.len();

        for (idx, context) in contexts.into_iter().enumerate() {
            self.render_label_context(f, context, severity)?;

            // Unless we're at the last label, print a vertical break in the gutter.
            if idx < count - 1 {
                self.render_snippet_breakln(f, gutter_size)?;
            }
        }

        // Render the footer of the label group.
        //
        //    ╰──
        //
        self.render_snippet_footer(f, gutter_size)
    }

    /// Renders the header of a source snippet.
    ///
    /// ```text
    //    ╭─[std/array.lm:35:8]
    /// ```
    fn render_snippet_header(
        &self,
        f: &mut impl std::fmt::Write,
        name: Option<&str>,
        padding: usize,
        line: usize,
        column: usize,
    ) -> std::fmt::Result {
        self.write_ident(f)?;

        write!(
            f,
            "{}{}{}",
            " ".repeat(padding),
            self.theme.arrows.top_left,
            self.theme.arrows.hbar,
        )?;

        if let Some(name) = name {
            self.render_source_path(f, name, line + 1, column)
        } else {
            writeln!(
                f,
                "{}",
                std::iter::repeat_n(self.theme.arrows.hbar, 10).collect::<String>()
            )
        }
    }

    /// Renders the gutter for a single line in a source snippet.
    ///
    /// ```text
    //    28 │
    /// ```
    fn render_snippet_gutter(
        &self,
        f: &mut impl std::fmt::Write,
        padding: usize,
        gutter: impl std::fmt::Display,
        bar: impl std::fmt::Display,
    ) -> std::fmt::Result {
        self.write_ident(f)?;

        write!(f, "{gutter:^padding$}{bar} ")
    }

    /// Renders an empty gutter for a single line in a source snippet.
    ///
    /// ```text
    //       │
    /// ```
    fn render_snippet_line_empty_gutter(&self, f: &mut impl std::fmt::Write, padding: usize) -> std::fmt::Result {
        self.render_snippet_gutter(f, padding, "", self.theme.arrows.vertical)
    }

    /// Renders the gutter for a single line in a source snippet.
    ///
    /// ```text
    //    28 │
    /// ```
    fn render_snippet_line_gutter(
        &self,
        f: &mut impl std::fmt::Write,
        padding: usize,
        line_num: usize,
    ) -> std::fmt::Result {
        self.render_snippet_gutter(
            f,
            padding,
            self.style(&line_num, self.theme.style.gutter),
            self.theme.arrows.vertical,
        )
    }

    /// Renders a single line in a source snippet.
    ///
    /// ```text
    //    28 │    /// When creating an array with a set capacity, it's length will still be zero.
    /// ```
    fn render_snippet_line(
        &self,
        f: &mut impl std::fmt::Write,
        padding: usize,
        line: impl std::fmt::Display,
        line_num: usize,
    ) -> std::fmt::Result {
        self.render_snippet_line_gutter(f, padding, line_num)?;

        writeln!(f, "{line}")
    }

    /// Renders a single vertical break in a source snippet.
    ///
    /// ```text
    //      ∶
    /// ```
    fn render_snippet_break(&self, f: &mut impl std::fmt::Write, padding: usize) -> std::fmt::Result {
        self.render_snippet_gutter(f, padding, "", self.theme.arrows.vertical_break)
    }

    /// Renders a single vertical break in a source snippet.
    ///
    /// ```text
    //      ∶
    /// ```
    fn render_snippet_breakln(&self, f: &mut impl std::fmt::Write, padding: usize) -> std::fmt::Result {
        self.render_snippet_gutter(f, padding, "", self.theme.arrows.vertical_break)?;

        writeln!(f)
    }

    /// Renders the footer of a source snippet.
    ///
    /// ```text
    //    ╰──
    /// ```
    fn render_snippet_footer(&self, f: &mut impl std::fmt::Write, padding: usize) -> std::fmt::Result {
        self.write_ident(f)?;
        write_padding(f, padding)?;

        writeln!(
            f,
            "{}{}",
            self.theme.arrows.bottom_left,
            std::iter::repeat_n(self.theme.arrows.hbar, 2).collect::<String>()
        )
    }

    /// Renders the path of the source file.
    ///
    /// ```text
    ///   std/array.lm:35:8
    /// ```
    fn render_source_path(
        &self,
        f: &mut impl std::fmt::Write,
        name: &str,
        line: usize,
        column: usize,
    ) -> std::fmt::Result {
        writeln!(
            f,
            "[{}:{}:{}]",
            self.style(&name, self.theme.style.link),
            line,
            column + 1
        )
    }

    /// Renders the footer of a diagnostic message.
    ///
    /// # Example
    ///
    /// ```text
    ///   help: doc comments are only allowed on definitions
    ///   help: you can use triple forward-slash to denote doc comments
    /// ```
    fn render_footer(&self, f: &mut impl std::fmt::Write, diagnostic: &dyn Diagnostic) -> std::fmt::Result {
        if let Some(help) = diagnostic.help() {
            for line in help {
                self.render_help(f, &line)?;
            }
        }

        Ok(())
    }

    /// Renders a single help message, which is attached to a diagnostic
    /// message.
    ///
    /// # Example
    ///
    /// Single line help message:
    /// ```text
    ///   help: did you mean 'invoke'?
    /// ```
    ///
    /// Multi-line help message:
    /// ```text
    ///   help: did you mean 'invoke'?
    ///         ... or perhaps 'invoke_all'?
    /// ```
    ///
    /// Optionally with a suggestion attached:
    /// ```text
    ///    help: consider removing these parenthesis
    ///  34 │         return (0..10);
    ///     |                ^     ^
    /// ```
    fn render_help(&self, f: &mut impl std::fmt::Write, help: &Help) -> std::fmt::Result {
        let help_gutter = "   help: ";
        let help_padding = help_gutter.to_string().len();

        // If the help message has multiple lines, we need to indent the other lines
        // with the same padding, so it lines up correctly.
        //
        // So, instead of
        // ```text
        //   help: expected type `Array<T>`
        // found type `Boolean`
        // ```
        //
        // we would print:
        // ```text
        //   help: expected type `Array<T>`
        //         found type `Boolean`
        // ```
        for (i, line) in help.message.lines().enumerate() {
            self.write_ident(f)?;

            if i == 0 {
                writeln!(f, "{}{}", self.style(&help_gutter, self.theme.style.help), line)?;
            } else {
                writeln!(f, "{}{}", " ".repeat(help_padding), line)?;
            }
        }

        let mut padding = 0;
        let mut suggestion_groups: IndexMap<Option<String>, Vec<Suggestion>> = IndexMap::new();

        for suggestion in &help.suggestions {
            let source = suggestion.source();
            let source_name = source.name().map(|n| n.to_string());
            let source_content = source.content();

            padding = padding.max(self.gutter_size_of(&source_content));

            if let Some(group) = suggestion_groups.get_mut(&source_name) {
                group.push(suggestion.clone());
            } else {
                suggestion_groups.insert(source_name, vec![suggestion.clone()]);
            }
        }

        for (_, suggestions) in suggestion_groups {
            self.render_suggestion_group(f, &suggestions, padding)?;
        }

        Ok(())
    }

    /// Renders a group of suggestions defined within a help message, where
    /// all suggestions share the same source file.
    ///
    /// # Example
    ///
    /// ```text
    ///    help: consider removing these parenthesis
    ///  24 │         return (0..10);
    ///     |                ^     ^
    //      ∶
    ///  39 │         return (start..end);
    ///     |                ^          ^
    /// ```
    fn render_suggestion_group(
        &self,
        f: &mut impl std::fmt::Write,
        suggestions: &[Suggestion],
        padding: usize,
    ) -> std::fmt::Result {
        if suggestions.is_empty() {
            return Ok(());
        }

        let first_suggestion = suggestions.first().unwrap().clone();
        let source = first_suggestion.source();
        let source_content = source.content();

        let mut suggested_lines: IndexMap<usize, Vec<Suggestion>> = IndexMap::new();

        for suggestion in suggestions {
            let start_idx = match &suggestion {
                Suggestion::Insertion { location, .. } => location.offset,
                Suggestion::Deletion { range } | Suggestion::Replacement { range, .. } => range.span.0.start,
            };

            let Coord { line, .. } = coords_of_idx(&source_content, start_idx);

            if let Some(group) = suggested_lines.get_mut(&line) {
                group.push(suggestion.clone());
            } else {
                suggested_lines.insert(line, vec![suggestion.clone()]);
            }
        }

        let suggestion_len = suggested_lines.len();

        for (index, (line, suggestions)) in suggested_lines.into_iter().enumerate() {
            self.render_suggestion_line(f, line, suggestions)?;

            // Unless we're at the last suggestion, print a vertical break in the gutter.
            if index < suggestion_len - 1 {
                self.render_snippet_breakln(f, padding)?;
            }
        }

        Ok(())
    }

    /// Renders a single line where one-or-more suggestions are defined.
    ///
    /// # Example
    ///
    /// ```text
    ///    help: consider removing these parenthesis
    ///  24 │         return (0..10);
    ///     |                ^     ^
    fn render_suggestion_line(
        &self,
        f: &mut impl std::fmt::Write,
        line_num: usize,
        mut suggestions: Vec<Suggestion>,
    ) -> std::fmt::Result {
        if suggestions.is_empty() {
            return Ok(());
        }

        // Sort all suggestions, so earlier suggestions come first in the vector.
        suggestions.sort();

        // Since styling alters the content of the line, we need to
        // style the line with each suggestion in reverse order, so it
        // has no effect on previous suggestions on the same line.
        suggestions.reverse();

        let first_suggestion = suggestions.first().unwrap();

        let source = first_suggestion.source();
        let source_content = source.content();
        let source_line = extract_with_context(&source_content, first_suggestion.span(), 0);
        let padding = self.gutter_size_of(&source_content);

        // Render the suggestion itself.
        //
        //  24 │         return (0..10);
        //

        let mut styled_line = Box::new(source_line) as Box<dyn std::fmt::Display>;

        for suggestion in &suggestions {
            let span = coords_of_span(&source_content, suggestion.span());

            styled_line = self.style_suggestion_line(suggestion, styled_line, span);
        }

        self.render_snippet_line(f, padding, styled_line, line_num + 1)?;

        // Render the arrows below the suggestions
        //
        //     |                ^     ^
        //

        self.render_snippet_gutter(f, padding, "", self.theme.arrows.vertical)?;

        // Un-reverse the suggestions again, so we can draw the arrows
        // below the marked sections of the suggestions.
        suggestions.reverse();

        let mut offset = 0;
        for suggestion in &suggestions {
            let span = suggestion.span();
            let Span { start, end } = coords_of_span(&source_content, span);

            // Write the padding between the arrows.
            let spacing = start.column.saturating_sub(offset);

            write!(f, "{}", " ".repeat(spacing))?;

            let style = match suggestion {
                Suggestion::Insertion { .. } | Suggestion::Replacement { .. } => self.theme.style.insertion,
                Suggestion::Deletion { .. } => self.theme.style.deletion,
            };

            let arrow_count = match suggestion {
                Suggestion::Insertion { value, .. } => value.len(),
                Suggestion::Replacement { replacement, .. } => replacement.len(),
                Suggestion::Deletion { range } => range.span.0.len(),
            };

            for _ in 0..arrow_count {
                write!(f, "{}", self.style(&self.theme.arrows.arrow_up, style))?;
            }

            offset = end.column;
        }

        writeln!(f)
    }

    /// Styles a single suggestion into a "fixed" line.
    fn style_suggestion_line<'a>(
        &self,
        suggestion: &Suggestion,
        line: Box<dyn std::fmt::Display + 'a>,
        span: Span,
    ) -> Box<dyn std::fmt::Display + 'a> {
        let line = line.to_string();

        let span: Range<usize> = if span.is_multiline() {
            span.start.column..line.len()
        } else {
            span.start.column..span.end.column
        };

        let formatted = match suggestion {
            Suggestion::Deletion { .. } => {
                let [before, middle, after] = split_str_at(&line, vec![span.start, span.end]);

                format!("{}{}{}", before, self.style(&middle, self.theme.style.deletion), after)
            }
            Suggestion::Insertion { value, .. } => {
                let [before, middle, after] = split_str_at(&line, vec![span.start, span.end]);

                format!(
                    "{}{}{}{}",
                    before,
                    self.style(&value, self.theme.style.insertion),
                    middle,
                    after
                )
            }
            Suggestion::Replacement { replacement, range } => {
                let length = range.span.0.len();
                let [before, _, after] = split_str_at(&line, vec![span.start, span.start + length]);

                format!(
                    "{}{}{}",
                    before,
                    self.style(&replacement, self.theme.style.insertion),
                    after
                )
            }
        };

        Box::new(formatted) as Box<dyn std::fmt::Display>
    }
}

/// Writes the the given amount of padding to the provided writer.
fn write_padding(f: &mut impl std::fmt::Write, padding: usize) -> std::fmt::Result {
    write!(f, "{}", " ".repeat(padding))
}

/// Groups a list of [`Label`]s into a tree of [`Label`]s, where each parent
/// label overlaps with all it's direct child nodes.
fn group_overlapping_labels(
    diag_source: Option<Arc<dyn Source>>,
    labels: impl Iterator<Item = Label>,
) -> Vec<LabelContext> {
    let mut labels = labels.into_iter().enumerate().collect::<Vec<(usize, Label)>>();
    labels.sort_unstable_by_key(|(_, l)| l.range().0.start);

    let mut contexts = Vec::with_capacity(labels.len());
    let mut visited = HashSet::new();

    for (idx, (pos, parent)) in labels.iter().cloned().enumerate() {
        // If no source code is attached to the label itself, see if
        // a source is attached to the parent diagnostic.
        //
        // If no source is found on either, skip over the label entirely.
        let Some(parent_source) = parent.source.clone().or(diag_source.clone()) else {
            continue;
        };

        if !visited.insert(idx) {
            continue;
        }

        let parent_span = parent.range.0.clone();
        let mut context = LabelContext {
            pos,
            parent,
            children: Vec::new(),
            source: parent_source.clone(),
        };

        // If the parent label only spans a single line, it cannot contain any children.
        if !coords_of_span(parent_source.content().as_ref(), parent_span.clone()).is_multiline() {
            contexts.push(context);

            continue;
        }

        for (idx, (pos, child)) in labels.iter().enumerate().skip(idx + 1) {
            let Some(child_source) = child.source.clone().or(diag_source.clone()) else {
                continue;
            };

            // Group the labels into groups where all elements have the same source file.
            // This helps prevent multiple label headers in a row from defining the same
            // file path.
            if child_source.name() != parent_source.name() {
                continue;
            }

            if parent_span.contains(&child.range.0.start) && visited.insert(idx) {
                context.children.push((*pos, child.clone()));
            }
        }

        contexts.push(context);
    }

    // Sort the labels back to their original ordering.
    contexts.sort_unstable_by_key(|c| c.pos);

    for context in &mut contexts {
        context.children.sort_unstable_by_key(|(pos, _)| *pos);
    }

    contexts
}

#[derive(Debug)]
struct LabelContext {
    /// Used for reordering and sorting.
    pub pos: usize,

    /// Defines the root label within the context.
    pub parent: Label,

    /// Defines all child labels, which are contained within the parent.
    pub children: Vec<(usize, Label)>,

    /// Defines the common source for the labels.
    pub source: Arc<dyn Source>,
}

impl LabelContext {
    /// Gets the span which contains all labels within the context, including
    /// the parent.
    pub fn max_span(&self) -> SpanRange {
        let start = self.parent.range.0.start;
        let end = self.children.iter().map(|(_, c)| c.range.0.end).max().unwrap_or(0);

        SpanRange(start..end.max(self.parent.range().0.end))
    }
}

struct LabelGroup {
    /// Defines all the labels in the group
    pub labels: Vec<Label>,

    /// Defines the common source for the labels
    pub source: Arc<dyn Source>,
}

/// Defines a text span, where each character can be individually styled.
#[derive(Debug, Clone)]
struct StyledText {
    str: String,
    chars: Vec<Style>,
}

impl StyledText {
    pub fn new(str: String) -> Self {
        Self {
            chars: vec![Style::new(); str.len()],
            str,
        }
    }

    /// Appends the given string, without any specific styling.
    pub fn append(&mut self, str: &str, style: Style) {
        self.chars.extend(vec![style; str.len()]);
        self.str.push_str(str);
    }

    /// Applies a style to a span of characters.
    pub fn style_span(&mut self, span: Range<usize>, style: Style) {
        for idx in span {
            let Some(s) = self.chars.get_mut(idx) else {
                break;
            };

            *s = style;
        }
    }

    pub fn render(self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        for (c, style) in self.str.chars().zip(self.chars) {
            write!(f, "{}", c.style(style))?;
        }

        Ok(())
    }
}

impl Display for StyledText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.clone().render(f)
    }
}

/// Changes a single character inside the given [`String`], at the offset
/// `offset`.
///
/// The offset defines a character offset, not a byte offset. The function
/// supports UTF-8, but it does come at the cost of having a time-complexity of
/// **O(n)**, where `n` is the given offset.
fn str_set_char(str: &mut String, offset: usize, c: char) -> bool {
    let Some(char_range) = str.char_indices().nth(offset).map(|(i, c)| i..i + c.len_utf8()) else {
        return false;
    };

    str.replace_range(char_range, &c.to_string());

    true
}

/// Splits the given string into `N` slices, where each index defines
/// where the source string should be split.
fn split_str_at<const N: usize>(str: &str, mut indices: Vec<usize>) -> [&str; N] {
    indices.sort_unstable();
    indices.reverse();

    let mut current = str;
    let mut slices = [""; N];

    for (i, index) in indices.iter().enumerate() {
        let (before, after) = current.split_at(*index);

        current = before;
        slices[N - i - 1] = after;
    }

    slices[0] = current;

    slices
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Coord {
    pub line: usize,
    pub column: usize,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Span {
    pub start: Coord,
    pub end: Coord,
}

impl Span {
    pub fn columns(self) -> Range<usize> {
        debug_assert_eq!(self.start.line, self.end.line);

        if self.start.column > self.end.column {
            return self.start.column..self.start.column + 1;
        }

        self.start.column..self.end.column
    }

    pub fn is_multiline(self) -> bool {
        self.start.line != self.end.line
    }
}

/// Gets the line number and column indices which contains the given span.
fn coords_of_span(str: &str, span: impl Into<Range<usize>>) -> Span {
    let range: Range<usize> = span.into();

    let start = coords_of_idx(str, range.start);
    let end = coords_of_idx(str, range.end);

    Span { start, end }
}

/// Gets the line number and column number which contains the character at the
/// given index.
fn coords_of_idx(str: &str, index: usize) -> Coord {
    if index > str.len() {
        let line_cnt = str.lines().count();

        return Coord {
            line: line_cnt.saturating_sub(1),
            column: str.lines().last().map(|l| l.len()).unwrap_or_default(),
        };
    }

    let mut line = 0;
    let mut column = 0;

    for (i, c) in str.chars().peekable().enumerate() {
        if i == index {
            return Coord { line, column };
        }

        if c == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }

    if index == str.len() {
        return Coord { line, column };
    }

    Coord::default()
}

#[cfg(test)]
mod coords_of_idx_tests {
    use super::{Coord, coords_of_idx};

    #[test]
    fn test_index_out_of_range() {
        let source = "let a = 1;";
        let Coord { line, column } = coords_of_idx(source, 12);

        assert_eq!(line, 0);
        assert_eq!(column, 10);
    }

    #[test]
    fn test_index_at_end_boundary() {
        let source = "let a = 1;";
        let Coord { line, column } = coords_of_idx(source, 10);

        assert_eq!(line, 0);
        assert_eq!(column, 10);
    }

    #[test]
    fn test_multiline() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let Coord { line, column } = coords_of_idx(source, 26);

        assert_eq!(line, 2);
        assert_eq!(column, 4);
    }

    #[test]
    fn test_multiline_line_boundary_start() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let Coord { line, column } = coords_of_idx(source, 22);

        assert_eq!(line, 2);
        assert_eq!(column, 0);
    }

    #[test]
    fn test_multiline_line_boundary_end() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let Coord { line, column } = coords_of_idx(source, 36);

        assert_eq!(line, 2);
        assert_eq!(column, 14);
    }
}

/// Extracts a slice of the given string, which contains the lines where
/// `span` is contained, along with the `context_lines` amount of surrounding
/// lines.
///
/// # Example
///
/// ```
/// use lume_errors::render::graphical::extract_with_context;
///
/// let source = r#"let a = 1;
/// let b = 2;
/// let c = a + b;
/// let d = c * 2;
/// let e = (d + 3) * 2;"#;
///
/// // indexes "a + b" on line 3
/// let span = 30..35;
///
/// let snipped = extract_with_context(source, span, 1);
///
/// assert_eq!(snipped, r#"let b = 2;
/// let c = a + b;
/// let d = c * 2;"#);
/// ```
pub fn extract_with_context(input: &str, range: impl Into<Range<usize>>, context_lines: usize) -> &str {
    let (slice, _) = extract_with_context_offset(input, range, context_lines);

    slice
}

/// Extracts a slice of the given string, which contains the lines where
/// `span` is contained, along with the `context_lines` amount of surrounding
/// lines.
///
/// The function also returns the line number where the "center" of the context
/// starts.
///
/// # Example
///
/// ```
/// use lume_errors::render::graphical::extract_with_context;
///
/// let source = r#"let a = 1;
/// let b = 2;
/// let c = a + b;
/// let d = c * 2;
/// let e = (d + 3) * 2;"#;
///
/// // indexes "a + b" on line 3
/// let span = 30..35;
///
/// let snipped = extract_with_context(source, span, 1);
///
/// assert_eq!(snipped, r#"let b = 2;
/// let c = a + b;
/// let d = c * 2;"#);
/// ```
pub fn extract_with_context_offset(input: &str, range: impl Into<Range<usize>>, context_lines: usize) -> (&str, usize) {
    let range: Range<usize> = range.into();

    let mut line_start = 0;
    let mut line_spans = Vec::new();

    for line in input.lines() {
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

        return (&input[0..last_line_idx], context_lines);
    }

    let first_matching_line = *matching_lines.first().unwrap();

    let first_match = first_matching_line.saturating_sub(context_lines);
    let last_match = (matching_lines.last().unwrap() + context_lines).min(line_spans.len() - 1);

    let start_byte = line_spans[first_match].start;
    let end_byte = line_spans[last_match].end;

    (&input[start_byte..end_byte], first_matching_line)
}

#[cfg(test)]
mod extract_with_context_offset_tests {
    use super::extract_with_context_offset;

    #[test]
    fn test_extract_with_context() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 30..35, 1);

        assert_eq!(snipped, "let b = 2;\nlet c = a + b;\nlet d = c * 2;");
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_extract_without_context() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 30..35, 0);

        assert_eq!(snipped, "let c = a + b;");
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_extract_at_beginning_boundary() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 0..10, 2);

        assert_eq!(snipped, "let a = 1;\nlet b = 2;\nlet c = a + b;");
        assert_eq!(offset, 0);
    }

    #[test]
    fn test_extract_at_ending_boundary() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 60..71, 2);

        assert_eq!(snipped, "let c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;");
        assert_eq!(offset, 4);
    }

    #[test]
    fn test_extract_at_line_start() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 22..36, 1);

        assert_eq!(snipped, "let b = 2;\nlet c = a + b;\nlet d = c * 2;");
        assert_eq!(offset, 2);
    }

    #[test]
    fn test_extract_first_line() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 4..9, 1);

        assert_eq!(snipped, "let a = 1;\nlet b = 2;");
        assert_eq!(offset, 0);
    }

    #[test]
    fn test_extract_last_line() {
        let source = "let a = 1;\nlet b = 2;\nlet c = a + b;\nlet d = c * 2;\nlet e = (d + 3) * 2;";
        let (snipped, offset) = extract_with_context_offset(source, 64..75, 1);

        assert_eq!(snipped, "let d = c * 2;\nlet e = (d + 3) * 2;");
        assert_eq!(offset, 4);
    }
}
