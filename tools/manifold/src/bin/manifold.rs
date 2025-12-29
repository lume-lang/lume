use clap::Parser;
use lume_errors::DiagCtx;

fn main() {
    let config = manifold::Config::parse();
    let dcx = DiagCtx::new();

    match manifold::manifold_entry(config, dcx.clone()) {
        Ok(code) => std::process::exit(code),
        Err(err) => {
            dcx.emit(err);

            let mut renderer = error_snippet::GraphicalRenderer::new();
            renderer.use_colors = true;
            renderer.highlight_source = true;

            dcx.render_stderr(&mut renderer);
            dcx.clear();
        }
    }
}
