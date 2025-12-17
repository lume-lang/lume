use std::str::FromStr;

use lsp_server::RequestId;
use lsp_types::*;
use lume_errors::Result;

use crate::listen::FileLocation;
use crate::state::State;

pub(crate) fn on_hover(state: &State, id: RequestId, location: FileLocation) -> Result<()> {
    let Some(location) = state.location_of(&location) else {
        state.err(id, lsp_server::ErrorCode::InvalidParams, "document not available")?;
        return Ok(());
    };

    let content = match crate::symbols::hover::hover_content_of(state, location) {
        Ok(content) => content,
        Err(err) => {
            log::error!("could not retrieve content: {}", err.message());

            state.err(
                id,
                lsp_server::ErrorCode::RequestFailed,
                &format!("could not retrieve content: {}", err.message()),
            )?;
            return Ok(());
        }
    };

    state.ok(id, &Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    })?;

    Ok(())
}

pub(crate) fn go_to_definition(state: &State, id: RequestId, location: FileLocation) -> Result<()> {
    let Some(location) = state.location_of(&location) else {
        state.err(id, lsp_server::ErrorCode::RequestFailed, "document not available")?;
        return Ok(());
    };

    let Some(definition_location) = crate::symbols::definition::definition_of(state, location)? else {
        state.err(
            id,
            lsp_server::ErrorCode::RequestFailed,
            "could not find symbol definition",
        )?;
        return Ok(());
    };

    let definition_uri = Uri::from_str(&format!("file://{}", definition_location.file.name)).unwrap();

    state.ok(
        id,
        &GotoDefinitionResponse::Scalar(lsp_types::Location {
            uri: definition_uri,
            range: crate::location_range(definition_location),
        }),
    )?;

    Ok(())
}
