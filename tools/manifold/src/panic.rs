use std::backtrace::{Backtrace, BacktraceStatus};
use std::cell::Cell;
use std::fmt::{Display, Write};
use std::panic::PanicHookInfo;
use std::sync::{Arc, Mutex};

type PanicHook = Box<dyn Fn(&PanicHookInfo<'_>) + Sync + Send + 'static>;
type CaptureBuf = Arc<Mutex<String>>;

thread_local!(
    static CAPTURE_BUF: Cell<Option<CaptureBuf>> = const { Cell::new(None) };
);

pub(crate) fn set_capture_buf(buf: CaptureBuf) {
    CAPTURE_BUF.set(Some(buf));
}

pub(crate) fn take_capture_buf() -> Option<CaptureBuf> {
    CAPTURE_BUF.take()
}

/// Installs a custom panic hook to capture panic output and save it
/// to a thread-local buffer.
pub(crate) fn install_panic_hook() {
    let default = std::panic::take_hook();

    std::panic::set_hook(Box::new(move |info| custom_panic_hook(&default, info)));
}

fn custom_panic_hook(default_hook: &PanicHook, info: &std::panic::PanicHookInfo<'_>) {
    // Temporarily taking the capture buffer means that if a panic occurs in
    // the subsequent code, that panic will fall back to the default hook.
    let Some(buf) = take_capture_buf() else {
        // There was no capture buffer, so delegate to the default hook.
        default_hook(info);
        return;
    };

    let mut out = buf.lock().unwrap_or_else(|e| e.into_inner());

    let payload = get_payload(info);
    let location = get_location(info);
    let backtrace = Backtrace::capture();

    writeln!(out, "test thread panicked at {location}:\n{payload}").unwrap();

    match backtrace.status() {
        BacktraceStatus::Captured => {
            write!(out, "stack backtrace:\n{backtrace}",).unwrap();
        }
        BacktraceStatus::Disabled => {
            writeln!(
                out,
                "note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace",
            )
            .unwrap();
        }
        _ => {}
    }

    drop(out);
    set_capture_buf(buf);
}

fn get_payload<'a>(info: &'a PanicHookInfo) -> &'a str {
    if let Some(s) = info.payload().downcast_ref::<&str>() {
        s
    } else if let Some(s) = info.payload().downcast_ref::<String>() {
        s.as_str()
    } else {
        "Box<dyn Any>"
    }
}

fn get_location<'a>(info: &'a PanicHookInfo<'_>) -> &'a dyn Display {
    match info.location() {
        Some(location) => location,
        None => &"(unknown)",
    }
}
