use crate::{Label, LumeDiagnostic};
use codespan_reporting as codespan;
use codespan_reporting::diagnostic::{Diagnostic, LabelStyle, Severity};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, ColorSpec, StandardStream};
use codespan_reporting::term::{self};

impl<'a> LumeDiagnostic<'a> {
    /// Renders the diagnostic message to the console.
    pub fn render(self) {
        let severity: Severity = self.severity.into();

        let mut diagnostic = Diagnostic::new(severity).with_message(self.message);
        let mut source_map: SimpleFiles<String, String> = SimpleFiles::new();

        if let Some(code) = self.code {
            diagnostic = diagnostic.with_code(code);
        }

        if let Some(labels) = self.labels {
            for (idx, label) in labels.into_iter().enumerate() {
                diagnostic = diagnostic.with_label(Self::create_label(idx, label, &mut source_map));
            }
        }

        for related in self.related {
            if let Some(labels) = related.labels {
                for (idx, label) in labels.into_iter().enumerate() {
                    diagnostic = diagnostic.with_label(Self::create_label(idx, label, &mut source_map));
                }
            }
        }

        if let Some(help) = self.help {
            diagnostic = diagnostic.with_notes(help);
        }

        let mut label_color = ColorSpec::new();
        label_color.set_fg(None);

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = term::Config {
            // We intentionally remove all color from the labels, since we want to be
            // able to highlight specific parts of the message.
            styles: term::Styles {
                primary_label_error: label_color.clone(),
                primary_label_warning: label_color.clone(),
                primary_label_note: label_color.clone(),
                primary_label_help: label_color.clone(),
                primary_label_bug: label_color.clone(),
                secondary_label: label_color,
                ..term::Styles::default()
            },
            ..term::Config::default()
        };

        term::emit(&mut writer.lock(), &config, &source_map, &diagnostic).unwrap();
    }

    fn create_label(
        idx: usize,
        label: Label,
        sources: &mut SimpleFiles<String, String>,
    ) -> codespan::diagnostic::Label<usize> {
        let source_name = label.source.name.to_string();
        let source_id = sources.add(source_name, label.source.content.clone());

        let style = if idx == 0 {
            LabelStyle::Primary
        } else {
            LabelStyle::Secondary
        };

        codespan::diagnostic::Label::new(style, source_id, label.range.0).with_message(&label.label)
    }
}
