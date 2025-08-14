use lume_errors::DiagCtx;

fn main() {
    let dcx = DiagCtx::new();

    if let Err(err) = manifold::manifold_entry() {
        dcx.emit(err);
    }

    let mut renderer = error_snippet::GraphicalRenderer::new();
    renderer.use_colors = true;
    renderer.highlight_source = true;

    dcx.render_stderr(&mut renderer);
    dcx.clear();
}
