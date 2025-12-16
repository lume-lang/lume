use lsp_types::{Position, Range};
use lume_span::Location;

pub(crate) fn location_range(loc: Location) -> Range {
    position_from_range(&loc.file.content, &loc.index)
}

pub(crate) fn position_from_range(text: &str, range: &std::ops::Range<usize>) -> Range {
    let start = position_from_index(text, range.start);
    let end = position_from_index(text, range.end);

    Range::new(start, end)
}

#[allow(clippy::cast_possible_truncation)]
pub(crate) fn position_from_index(text: &str, index: usize) -> Position {
    let mut line = 0;
    let mut line_start = 0;

    for (i, b) in text.bytes().enumerate() {
        if i == index {
            return Position::new(line, (i - line_start) as u32);
        }

        if b == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }

    Position::new(line, (index - line_start) as u32)
}
