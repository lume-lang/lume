use lume_errors::Result;
use lume_fmt::Config;

pub(crate) fn formatted_file(content: &str, config: Config) -> Result<Vec<lsp_types::TextEdit>> {
    let dcx = lume_errors::DiagCtx::new();
    let formatted_string = lume_fmt::format_src(content, &config, dcx.handle())?;

    let line_count = u32::try_from(content.lines().count()).unwrap();

    Ok(vec![lsp_types::TextEdit {
        range: lsp_types::Range {
            start: lsp_types::Position::new(0, 0),
            end: lsp_types::Position::new(line_count, 0),
        },
        new_text: formatted_string,
    }])
}

pub(crate) fn parse_formatting_config(options: lsp_types::FormattingOptions) -> Config {
    Config {
        indentation: lume_fmt::Indentation {
            use_tabs: !options.insert_spaces,
            tab_width: options.tab_size as usize,
        },
        trailing_line: options.insert_final_newline.unwrap_or(true),
        ..Default::default()
    }
}
