use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use lume_errors::{MapDiagnostic, Result};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::layer::SubscriberExt;

pub struct Options {
    /// Defines the default level filter for traces, if not overriden.
    pub default_filter: Option<LevelFilter>,

    /// If given, writes all enabled events and spans to the given log file.
    pub log_file: Option<PathBuf>,
}

/// Guard for flushing buffered logs to the output.
#[expect(unused, reason = "drop guard")]
pub struct Guard(tracing_appender::non_blocking::WorkerGuard);

/// Initializes the tracing subscriber, allowing for function tracing during
/// development.
///
/// The function uses the `LUMEC_LOG` environment variable as a filter,
/// defaulting back to `off` (no traces) if unset or invalid.
#[must_use = "returns guard for flushing log output"]
pub fn init_subscriber(options: Options) -> Result<Guard> {
    let use_colors = options.log_file.is_none();

    let output = if let Some(log_file_path) = options.log_file {
        Box::new(FileTarget::open(log_file_path)?) as Box<dyn std::io::Write + Send>
    } else {
        Box::new(std::io::stdout()) as Box<dyn std::io::Write + Send>
    };

    let (non_blocking, guard) = tracing_appender::non_blocking(output);

    let filter_layer = EnvFilter::try_from_env("LUMEC_LOG")
        .or_else(|_| {
            EnvFilter::builder()
                .with_default_directive(LevelFilter::ERROR.into())
                .parse(if let Some(filter) = options.default_filter {
                    filter.to_string()
                } else {
                    String::from("off")
                })
        })
        .unwrap();

    let tree_layer = tracing_tree::HierarchicalLayer::new(2)
        .with_ansi(use_colors)
        .with_writer(non_blocking)
        .with_bracketed_fields(false)
        .with_indent_lines(true)
        .with_indent_amount(2)
        .with_thread_names(false)
        .with_thread_ids(false)
        .with_targets(true)
        .with_verbose_exit(false)
        .with_verbose_entry(false);

    let tracer = tracing_subscriber::registry().with(filter_layer).with(tree_layer);

    tracing::subscriber::set_global_default(tracer).unwrap();

    tracing_log::LogTracer::init().expect("failed log tracer setup");

    Ok(Guard(guard))
}

struct FileTarget {
    file: Arc<Mutex<std::fs::File>>,
}

impl FileTarget {
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        let file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_cause("failed to open log file")?;

        Ok(FileTarget {
            file: Arc::new(Mutex::new(file)),
        })
    }
}

impl std::io::Write for FileTarget {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self.file.lock().unwrap().write(buf) {
            Ok(written) => Ok(written),
            Err(_err) => Ok(0),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let mut file_handler = self.file.lock().unwrap();
        file_handler.flush()
    }
}
