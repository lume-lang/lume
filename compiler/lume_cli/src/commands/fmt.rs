use std::path::PathBuf;
use std::sync::Arc;

use error_snippet::{Label, WithSource};
use lume_errors::*;
use lume_fmt::Config;
use lume_span::{PackageId, SourceFile};

#[derive(Debug, clap::Parser)]
#[command(name = "fmt", about = "Formatter for Lume source code", long_about = None)]
pub struct FormatCommand {
    #[arg(help = "Defines which files to format")]
    pub files: Vec<String>,

    #[arg(long, help = "Path to the formatter config file", value_hint = clap::ValueHint::FilePath)]
    pub config: Option<PathBuf>,

    #[arg(short = 'w', long, help = "Write the formatted file in-place instead of printing")]
    pub write: bool,
}

impl FormatCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        let config = match read_config_file(self.config.clone()) {
            Ok(config) => config,
            Err(err) => {
                dcx.emit_and_push(err);
                return;
            }
        };

        for file_path in &self.files {
            if let Err(err) = self.format_file(PathBuf::from(file_path), &config) {
                dcx.emit_and_push(
                    SimpleDiagnostic::new(format!("error while formatting input file: {file_path}"))
                        .add_cause(err)
                        .into(),
                );
            }
        }
    }

    fn format_file(&self, input_path: PathBuf, config: &Config) -> Result<()> {
        let content = std::fs::read_to_string(&input_path).map_diagnostic()?;
        let formatted = lume_fmt::format_src(&content, config)?;

        if self.write {
            std::fs::write(&input_path, &formatted).map_diagnostic()?;
        } else {
            println!("{formatted}");
        }

        Ok(())
    }
}

fn read_config_file(config_path: Option<PathBuf>) -> Result<Config> {
    let Some(config_path) = config_path.or_else(find_config_file) else {
        return Ok(Config::default());
    };

    let content = match std::fs::read_to_string(&config_path) {
        Ok(config) => config,
        Err(err) => {
            return Err(SimpleDiagnostic::new("could not read config file")
                .add_cause(err.into_diagnostic())
                .into());
        }
    };

    toml::from_str::<Config>(&content).map_err(|err| {
        let source_label = Label::error(None, err.span().unwrap_or_default(), err.message());

        SimpleDiagnostic::new("failed to parse config")
            .with_label(source_label)
            .with_source(Arc::new(SourceFile::new(PackageId::empty(), config_path, content)))
            .into()
    })
}

fn find_config_file() -> Option<PathBuf> {
    let cwd = std::env::current_dir().ok()?;

    if cwd.join("lumefmt.toml").exists() {
        return Some(cwd.join("lumefmt.toml"));
    }

    None
}
