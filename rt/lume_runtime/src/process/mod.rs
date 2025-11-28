use std::fmt::Display;
use std::os::raw::c_char;

use crate::array::Array;

#[derive(Default)]
struct SymbolInfo<'sym> {
    pub idx: usize,
    pub ip: *const u8,
    pub name: Option<&'sym str>,
    pub file: Option<&'sym str>,
    pub line: Option<u32>,
}

impl Display for SymbolInfo<'_> {
    #[cfg(feature = "color")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use owo_colors::OwoColorize;
        use owo_colors::Stream::Stdout;

        write!(
            f,
            "  frame #{}: {:016p} {}",
            self.idx,
            self.ip.if_supports_color(Stdout, |t| t.yellow()),
            self.name.unwrap_or("??")
        )?;

        match (self.file, self.line) {
            (Some(file), Some(line)) => write!(
                f,
                " at {}:{}",
                file.if_supports_color(Stdout, |t| t.cyan()),
                line.if_supports_color(Stdout, |t| t.yellow())
            ),
            (Some(file), None) => write!(f, " in {}", file.if_supports_color(Stdout, |t| t.cyan())),
            (_, _) => Ok(()),
        }
    }

    #[cfg(not(feature = "color"))]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "  frame #{}: {:016p} {}",
            self.idx,
            self.ip,
            self.name.unwrap_or("??")
        )?;

        match (self.file, self.line) {
            (Some(file), Some(line)) => write!(f, " at {file}:{line}",),
            (Some(file), None) => write!(f, " in {file}",),
            (_, _) => Ok(()),
        }
    }
}

#[unsafe(export_name = "std::process::abort")]
pub extern "C" fn abort() {
    let mut idx = 0;

    println!("thread stacktrace:");

    backtrace::trace(|frame| {
        let mut resolved = false;
        idx += 1;

        backtrace::resolve(frame.ip(), |symbol| {
            let mut info = SymbolInfo {
                idx,
                ip: frame.ip().cast_const().cast(),
                ..Default::default()
            };

            // The lifetime of the symbol requires us to keep `info` in scope, if we
            // want to skip allocation via cloning.
            info.name = symbol.name().and_then(|n| n.as_str());
            info.file = symbol.filename().and_then(|n| n.file_name().unwrap().to_str());
            info.line = symbol.lineno();

            println!("{info}");
            resolved = true;
        });

        if !resolved {
            println!("{}", SymbolInfo {
                idx,
                ip: frame.ip().cast_const().cast(),
                ..Default::default()
            });
        }

        true
    });

    std::process::exit(255);
}

#[unsafe(export_name = "std::process::bail")]
pub extern "C" fn bail(fmt: *const c_char, args: *const Array<*const c_char>) {
    println!("thread bailed:");
    crate::io::println(fmt, args);

    println!();
    abort();
}
