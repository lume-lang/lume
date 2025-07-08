use crate::commands::project_or_cwd;

use lume_driver::Driver;
use lume_errors::DiagCtxHandle;
use lume_session::{MirPrinting, OptimizationLevel, Options};

#[derive(Debug, clap::Parser)]
#[command(name = "run", about = "Build and run a Lume project", long_about = None)]
pub struct RunCommand {
    #[arg(help = "Path to the project", value_name = "DIR", value_hint = clap::ValueHint::DirPath)]
    pub path: Option<std::path::PathBuf>,

    #[arg(long, help = "Print the type context before analyzing")]
    pub print_type_ctx: bool,

    #[arg(
        long,
        default_value = "none",
        default_missing_value = "pretty",
        help = "Print the generated MIR",
        num_args(0..=1),
        require_equals = true
    )]
    pub print_mir: MirPrinting,

    #[arg(long, help = "Print the generated LLVM IR")]
    pub print_llvm_ir: bool,

    #[arg(
        short = 'O',
        long = "optimize",
        default_value = "2",
        help = "Optimization level",
        value_parser = clap::builder::PossibleValuesParser::new(["0", "1", "2", "3", "s", "z"])
    )]
    pub optimize: String,
}

impl RunCommand {
    #[allow(clippy::needless_pass_by_value)]
    pub(crate) fn run(&self, dcx: DiagCtxHandle) {
        let input = if let Some(v) = self.path.as_ref() {
            project_or_cwd(Some(v))
        } else {
            project_or_cwd(None)
        };

        let project_path = match input {
            Ok(path) => path,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        let options = Options {
            print_type_context: self.print_type_ctx,
            print_mir: self.print_mir,
            print_llvm_ir: self.print_llvm_ir,
            optimize: match self.optimize.as_str() {
                "0" => OptimizationLevel::O0,
                "1" => OptimizationLevel::O1,
                "2" => OptimizationLevel::O2,
                "3" => OptimizationLevel::O3,
                "s" => OptimizationLevel::Os,
                "z" => OptimizationLevel::Oz,
                _ => unreachable!(),
            },
        };

        let driver = match Driver::from_root(&std::path::PathBuf::from(project_path), dcx.clone()) {
            Ok(driver) => driver,
            Err(err) => {
                dcx.emit(err);
                return;
            }
        };

        if let Err(err) = driver.build(options) {
            dcx.emit(err);
        }
    }
}
