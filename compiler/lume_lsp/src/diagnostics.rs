use std::fmt::Write as _;
use std::path::PathBuf;
use std::str::FromStr;

use lsp_server::Message;
use lsp_types::notification::*;
use lsp_types::*;

use crate::state::State;

pub const LSP_SOURCE_LUME: &str = "lume";

impl State {
    /// Drain all diagnostics from the inner diagnostics context to
    /// the language client.
    pub(crate) fn drain_dcx_diagnostics(&self) {
        self.dcx.with_iter(|diagnostics| {
            for diagnostic in diagnostics {
                self.publish_diagnostic(diagnostic.as_ref());
            }
        });

        // Clear all the diagnostics from the context, so they won't
        // be reported on the next drain either.
        self.dcx.clear();

        // Take all the files which had one-or-more diagnostics, but no longer do and
        // push an empty list of diagnostics to the client.
        let prev = self.error_files_prev.read().unwrap();
        let curr = self.error_files_curr.read().unwrap();

        for file_url in prev.difference(&curr) {
            self.publish_diagnostics_to_file(&[], file_url.clone());
        }
    }

    /// Publishes the given [`error_snippet::Diagnostic`] to the language
    /// client.
    pub(crate) fn publish_diagnostic(&self, diagnostic: &dyn error_snippet::Diagnostic) {
        let Some(labels) = diagnostic.labels() else {
            return;
        };

        let labels = labels
            .into_iter()
            .filter_map(|label| self.lower_diagnostic_label(&label))
            .collect::<Vec<_>>();

        for label in &labels {
            self.error_files_curr
                .write()
                .unwrap()
                .insert(label.location.uri.clone());
        }

        let Some((primary_label, related)) = labels.split_first() else {
            return;
        };

        let related_info = related
            .iter()
            .map(|related| DiagnosticRelatedInformation {
                location: related.location.clone(),
                message: related.message.clone(),
            })
            .collect();

        let severity = match diagnostic.severity() {
            error_snippet::Severity::Note | error_snippet::Severity::Info => DiagnosticSeverity::INFORMATION,
            error_snippet::Severity::Help => DiagnosticSeverity::HINT,
            error_snippet::Severity::Warning => DiagnosticSeverity::WARNING,
            error_snippet::Severity::Error => DiagnosticSeverity::ERROR,
        };

        let code = diagnostic.code().map(|code| NumberOrString::String(code.to_string()));

        let mut message = primary_label.message.clone();
        if let Some(help_notes) = diagnostic.help() {
            for help_note in help_notes {
                let _ = write!(message, "\n{}", help_note.message);
            }
        }

        let diag = Diagnostic {
            range: primary_label.location.range,
            severity: Some(severity),
            code,
            code_description: None,
            source: Some(String::from(LSP_SOURCE_LUME)),
            message,
            related_information: Some(related_info),
            tags: None,
            data: None,
        };

        self.publish_diagnostics_to_file(&[diag], primary_label.location.uri.clone());
    }

    /// Publishes the given [`DiagnosticDiagnostic`] to the given file.
    pub(crate) fn publish_diagnostics_to_file(&self, diag: &[Diagnostic], file: Uri) {
        let params = PublishDiagnosticsParams {
            uri: file,
            diagnostics: diag.to_vec(),
            version: None,
        };

        self.dispatcher
            .send(Message::Notification(lsp_server::Notification::new(
                PublishDiagnostics::METHOD.to_owned(),
                params,
            )))
            .unwrap();
    }

    /// Lower the given [`error_snippet::Label`] into a [`DiagnosticLabel`].
    ///
    /// If the label doesn't have any source content attached, [`None`] is
    /// returned.
    fn lower_diagnostic_label(&self, label: &error_snippet::Label) -> Option<DiagnosticLabel> {
        let source = label.source()?;
        let position = crate::position_from_range(source.content().as_ref(), &label.range().0);

        let file_path = PathBuf::from(source.name()?);

        // Canonicalize the path to an absolute path, if not already.
        let uri = if file_path.has_root() {
            let file_path = format!("file://{}", file_path.display());

            Uri::from_str(file_path.as_str()).unwrap()
        } else {
            let root = PathBuf::from(self.vfs.workspace_root.as_str());
            let absolute = root.join(file_path.as_os_str().to_str().unwrap());
            let file_path = format!("file://{}", absolute.display());

            Uri::from_str(file_path.as_str()).unwrap()
        };

        Some(DiagnosticLabel {
            location: Location { uri, range: position },
            message: label.message().to_owned(),
        })
    }
}

#[derive(Debug)]
struct DiagnosticLabel {
    pub location: Location,
    pub message: String,
}
