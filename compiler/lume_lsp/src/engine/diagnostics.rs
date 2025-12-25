use std::collections::HashSet;
use std::fmt::Write as _;
use std::path::PathBuf;
use std::sync::RwLock;

use crossbeam::channel::Sender;
use lsp_server::Message;
use lsp_types::notification::*;
use lsp_types::*;
use lume_errors::DiagCtx;

pub const LSP_SOURCE_LUME: &str = "lume";

#[derive(Default)]
pub(crate) struct Diagnostics {
    previous: RwLock<HashSet<Uri>>,
    current: RwLock<HashSet<Uri>>,

    workspace_root: PathBuf,

    pub(crate) dcx: DiagCtx,
}

impl Diagnostics {
    pub fn new(workspace_root: PathBuf) -> Self {
        Self {
            workspace_root,
            ..Default::default()
        }
    }

    /// Drain all diagnostics from the inner diagnostics context to
    /// the language client.
    pub(crate) fn drain_to(&mut self, sender: &Sender<lsp_server::Message>) {
        std::mem::take(&mut self.previous);
        std::mem::swap(&mut self.previous, &mut self.current);

        self.dcx.with_iter(|diagnostics| {
            for diagnostic in diagnostics {
                log::info!("publishing diagnostic: {}", diagnostic.message());

                if diagnostic.labels().is_some() || diagnostic.source_code().is_some() {
                    self.publish_diagnostic(sender, diagnostic.as_ref());
                } else {
                    publish_message(sender, diagnostic.as_ref());
                }
            }
        });

        // Clear all the diagnostics from the context, so they won't
        // be reported on the next drain either.
        self.dcx.clear();

        // Take all the files which had one-or-more diagnostics, but no longer do and
        // push an empty list of diagnostics to the client.
        let prev = self.previous.read().unwrap();
        let curr = self.current.read().unwrap();

        for file_url in prev.difference(&curr) {
            publish_diagnostics_to_file(sender, &[], file_url.clone());
        }
    }

    /// Publishes the given [`error_snippet::Diagnostic`] to the language
    /// client.
    fn publish_diagnostic(&self, sender: &Sender<lsp_server::Message>, diagnostic: &dyn error_snippet::Diagnostic) {
        let Some(labels) = diagnostic.labels() else {
            return;
        };

        let labels = labels
            .into_iter()
            .filter_map(|label| self.lower_diagnostic_label(&label))
            .collect::<Vec<_>>();

        for label in &labels {
            self.current.write().unwrap().insert(label.location.uri.clone());
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

        let diag = lsp_types::Diagnostic {
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

        publish_diagnostics_to_file(sender, &[diag], primary_label.location.uri.clone());
    }

    /// Lower the given [`error_snippet::Label`] into a [`DiagnosticLabel`].
    ///
    /// If the label doesn't have any source content attached,
    /// returns [`None`].
    fn lower_diagnostic_label(&self, label: &error_snippet::Label) -> Option<DiagnosticLabel> {
        let source = label.source()?;

        let position = crate::position_from_range(source.content().as_ref(), &label.range().0);
        let file_path = PathBuf::from(source.name()?);

        // Canonicalize the path to an absolute path, if not already.
        let uri = if file_path.has_root() {
            crate::path_to_uri(&file_path)
        } else {
            crate::path_to_uri(&self.workspace_root.join(file_path))
        };

        Some(DiagnosticLabel {
            location: Location { uri, range: position },
            message: label.message().to_owned(),
        })
    }
}

/// Publishes the given [`DiagnosticDiagnostic`] to the given file.
fn publish_diagnostics_to_file(sender: &Sender<lsp_server::Message>, diag: &[lsp_types::Diagnostic], file: Uri) {
    let params = PublishDiagnosticsParams {
        uri: file,
        diagnostics: diag.to_vec(),
        version: None,
    };

    sender
        .send(Message::Notification(lsp_server::Notification::new(
            PublishDiagnostics::METHOD.to_owned(),
            params,
        )))
        .unwrap();
}

/// Publishes the given [`error_snippet::Diagnostic`] message to the
/// language client.
fn publish_message(sender: &Sender<lsp_server::Message>, diagnostic: &dyn error_snippet::Diagnostic) {
    let severity = match diagnostic.severity() {
        error_snippet::Severity::Note | error_snippet::Severity::Help | error_snippet::Severity::Info => return,
        error_snippet::Severity::Warning => lsp_types::MessageType::WARNING,
        error_snippet::Severity::Error => lsp_types::MessageType::ERROR,
    };

    let params = ShowMessageParams {
        typ: severity,
        message: diagnostic.message(),
    };

    sender
        .send(Message::Notification(lsp_server::Notification::new(
            ShowMessage::METHOD.to_owned(),
            params,
        )))
        .unwrap();
}

#[derive(Debug)]
struct DiagnosticLabel {
    pub location: Location,
    pub message: String,
}
