use iter_tools::Itertools;

/// Wraps the given text block to fit a line length of `line_length`. Newlines
/// inside the string will be preserved, if the satisfy the maximum line length.
/// The returned value is a joined string of each formatted text line, with
/// newlines where the line should break.
///
/// The implementation is a greedy algorithm; it will attempt to put as many
/// words on a single line as possible, until they can no longer fit.
pub(crate) fn wrap_text_block(str: String, line_length: usize) -> String {
    let mut lines = split_lines(str);

    for line in &mut lines {
        // Don't wrap any code inside of code blocks.
        if line.kind == LineKind::Code {
            continue;
        }

        line.content = wrap_line(std::mem::take(&mut line.content), line_length);
    }

    lines.into_iter().map(|line| line.content).collect_vec().join("")
}

#[derive(PartialEq, Eq)]
struct Line {
    pub content: String,
    pub kind: LineKind,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LineKind {
    /// The line is a standard source line and should be wrapped.
    Standard,

    /// The line is a line within a code block and should *not* be wrapped or
    /// otherwise formatted.
    Code,
}

/// Heuristically split the lines within the given string into a vector of
/// lines. The function will collapse multiple empty lines into a single empty
/// line, as well as mark code blocks.
fn split_lines(str: String) -> Vec<Line> {
    if str.find('\n').is_none() {
        return vec![Line {
            content: str,
            kind: LineKind::Standard,
        }];
    }

    let mut lines: Vec<Line> = Vec::new();
    let mut splits = str.split('\n').peekable();

    let mut inside_code_block = false;

    while let Some(line) = splits.next() {
        // Collapse multiple empty lines into a single empty line. Ignored inside code
        // blocks.
        if line.trim().is_empty() && !inside_code_block {
            while let Some(next) = splits.peek()
                && next.trim().is_empty()
            {
                let _ = splits.next();
            }

            if let Some(prev_line) = lines.last_mut()
                && prev_line.kind == LineKind::Standard
            {
                prev_line.content.push('\n');
            }

            continue;
        }

        if line.contains("```") {
            inside_code_block = !inside_code_block;
        }

        let mut line = line.to_string();
        line.push('\n');

        let kind = if inside_code_block {
            LineKind::Code
        } else {
            LineKind::Standard
        };

        lines.push(Line { content: line, kind });
    }

    lines
}

/// Wraps the given string to fit a line length of `line_length`. The returned
/// value is a joined string with line breaks.
fn wrap_line(str: String, line_length: usize) -> String {
    // If the given string is already short enough, we can return it as a single
    // line.
    if str.len() <= line_length {
        return str;
    }

    let mut line_start = 0;
    let mut current_offset = 0;
    let mut lines = Vec::new();

    let mut space_left = line_length;

    // Find the next word in the source text. `current_offset` is the index of the
    // last word separator found in the string plus 1. This prevents the same
    // character from being returned.
    while let Some(space_offset) = &str[current_offset..].find(' ') {
        // If the current line cannot hold the word, add the line content to `lines`.
        if space_offset + 1 > space_left {
            // Push the current string slice, starting at the current line index to the end
            // of the found word, sans the whitespace.
            lines.push(String::from(&str[line_start..current_offset - 1]));

            // Move the line start index forward to right after the whitespace.
            line_start = current_offset + space_offset + 1;
            space_left = line_length;
        } else {
            // If the word fits on the current line, subtract the word length plus size of
            // the whitespace.
            space_left -= space_offset + 1;
        }

        // Move the current offset forward to right after the found whitespace.
        current_offset += space_offset + 1;
    }

    // Push any remaining text onto the output buffer.
    lines.push(String::from(&str[line_start..]));

    lines.join("\n")
}
