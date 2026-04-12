use std::path::{Path, PathBuf};

use lume_errors::{IntoDiagnostic, MapDiagnostic, Result, SimpleDiagnostic};

use crate::error::*;

/// Create a new package
#[derive(Debug, clap::Parser)]
#[command(name = "new", long_about = None)]
pub struct NewCommand {
    #[arg(default_value = "console")]
    pub template: Template,

    /// Name of the package
    #[arg(long, short = 'n')]
    pub name: Option<String>,

    /// Path of the output directory
    #[arg(long, short = 'o', default_value = ".")]
    pub output: PathBuf,

    /// Skip creating a .gitignore file
    #[arg(long)]
    pub skip_gitignore: bool,

    /// Force overwrite existing files
    #[arg(long)]
    pub force: bool,
}

#[derive(clap::ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Template {
    /// Minimal executable binary package.
    Console,

    /// Namespace library package with a single type
    Library,
}

#[derive(askama::Template)]
#[template(path = "Arcfile")]
struct ArcfileTemplate<'a> {
    package_name: &'a str,
}

#[derive(askama::Template)]
#[template(path = "console/main.lm", escape = "none")]
struct ConsoleMainTemplate {}

#[derive(askama::Template)]
#[template(path = "library/lib.lm", escape = "none")]
struct LibraryLibTemplate<'a> {
    package_name: &'a str,
}

#[derive(askama::Template)]
#[template(path = ".gitignore", escape = "none")]
struct GitignoreTemplate {}

impl NewCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self) -> Result<()> {
        let output_directory = match std::env::current_dir() {
            Ok(dir) => dir.join(&self.output),
            Err(err) => {
                return Err(CouldNotDetermineCurrentDir {
                    inner: err.into_diagnostic(),
                }
                .into());
            }
        };

        if let Ok(mut readdir) = std::fs::read_dir(&output_directory)
            && readdir.next().is_some()
            && !self.force
        {
            return Err(SimpleDiagnostic::new("output directory is not empty")
                .with_help("to create the template anyway, run the command with `--force`"))?;
        }

        let templater = Templater::create(&output_directory)?;

        let package_name = if let Some(name) = &self.name {
            name.clone()
        } else {
            let canonicalized_path =
                std::fs::canonicalize(&output_directory).map_cause("failed to canonicalize output directory")?;

            let dir_name = canonicalized_path.file_name().expect("expected directory name");
            dir_name.to_string_lossy().to_string()
        };

        templater.render("Arcfile", ArcfileTemplate {
            package_name: &package_name,
        })?;

        match self.template {
            Template::Console => {
                templater.render("src/main.lm", ConsoleMainTemplate {})?;
            }
            Template::Library => {
                templater.render("src/lib.lm", LibraryLibTemplate {
                    package_name: &package_name,
                })?;
            }
        }

        if !self.skip_gitignore {
            templater.render(".gitignore", GitignoreTemplate {})?;
        }

        Ok(())
    }
}

struct Templater<'a> {
    root: &'a Path,
}

impl<'a> Templater<'a> {
    /// Creates a new template, rooted in the given directory.
    pub fn create(root: &'a Path) -> Result<Self> {
        std::fs::create_dir_all(root).map_cause("could not create package directory")?;

        Ok(Self { root })
    }

    /// Renders a template to the given path, relative to the root directory.
    pub fn render<P: AsRef<Path>, T: askama::Template>(&self, path: P, template: T) -> Result<()> {
        let dest_path = self.root.join(path);

        if let Some(parent_dir) = dest_path.parent() {
            std::fs::create_dir_all(parent_dir).map_cause("could not create templated directory")?;
        }

        if let Err(err) = std::fs::write(&dest_path, template.render().unwrap()) {
            return Err(
                SimpleDiagnostic::new(format!("failed to write templated file: {}", dest_path.display()))
                    .add_cause(err),
            )?;
        }

        Ok(())
    }
}
