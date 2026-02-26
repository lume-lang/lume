use lume_errors::Result;

use crate::server::Server;

pub enum ServerStatusNotification {}

impl lsp_types::notification::Notification for ServerStatusNotification {
    type Params = ServerStatusParams;

    const METHOD: &'static str = "experimental/serverStatus";
}

#[derive(serde::Deserialize, serde::Serialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ServerStatusParams {
    pub health: Health,
    pub message: Option<String>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "camelCase")]
pub enum Health {
    Ok,
    Warning,
    Error,
}

impl std::ops::BitOrAssign for Health {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = (*self).max(rhs);
    }
}

impl Server {
    pub(crate) fn current_status(&self) -> ServerStatusParams {
        use std::fmt::Write as _;

        let mut health = Health::Ok;
        let mut message = String::new();

        if self.engines.values().any(|engine| !engine.has_arcfile()) {
            health |= Health::Warning;

            let _ = writeln!(message, "Failed to load package:");
            let _ = writeln!(message, "Consider creating `Arcfile` in your package root(s):");

            for missing_root in self.engines.values().map(|engine| &engine.root) {
                let _ = writeln!(message, "- {}", missing_root.display());
            }

            let _ = writeln!(message);
        }

        ServerStatusParams {
            health,
            message: if message.is_empty() {
                None
            } else {
                Some(message.trim().to_string())
            },
        }
    }

    pub(crate) fn update_status(&mut self) -> Result<()> {
        let status = self.current_status();

        if self.experimental::<bool>("serverStatusNotification").is_some_and(|v| v) {
            self.notification::<ServerStatusNotification>(status)?;
        } else if status.health >= Health::Warning
            && let Some(message) = status.message
        {
            self.notification::<lsp_types::notification::ShowMessage>(lsp_types::ShowMessageParams {
                typ: match status.health {
                    Health::Ok => lsp_types::MessageType::INFO,
                    Health::Warning => lsp_types::MessageType::WARNING,
                    Health::Error => lsp_types::MessageType::ERROR,
                },
                message,
            })?;
        }

        Ok(())
    }
}
