use clap::ValueEnum;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::layer::SubscriberExt;

pub const LUMEC_LOG_ENV: &str = "LUMEC_LOG";
pub const LUMEC_TRACER_ENV: &str = "LUMEC_TRACER";

#[derive(ValueEnum, Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Tracer {
    #[default]
    Console,
    #[cfg(feature = "tracing-tracy")]
    Tracy,
}

pub fn init() {
    let is_log_set = if let Ok(log) = std::env::var(LUMEC_LOG_ENV)
        && !log.is_empty()
    {
        true
    } else {
        false
    };

    if let Ok(trace_env) = std::env::var(LUMEC_TRACER_ENV) {
        match trace_env.to_lowercase().as_str() {
            "console" | "stdout" if is_log_set => register_global_tracer(Tracer::Console),

            #[cfg(feature = "tracing-tracy")]
            "tracy" => register_global_tracer(Tracer::Tracy),

            _ => {}
        }
    } else if is_log_set {
        register_global_tracer(Tracer::Console)
    }
}

pub fn register_global_tracer(kind: Tracer) {
    match kind {
        Tracer::Console => register_console_tracer(),
        #[cfg(feature = "tracing-tracy")]
        Tracer::Tracy => register_tracy_tracer(),
    }
}

#[cfg(feature = "tracing-tracy")]
fn register_tracy_tracer() {
    let tracy_layer = tracing_tracy::TracyLayer::default();
    let tracer = tracing_subscriber::registry().with(tracy_layer);

    tracing::subscriber::set_global_default(tracer).unwrap();
}

fn register_console_tracer() {
    let format = tracing_subscriber::fmt::format()
        .pretty()
        .with_file(true)
        .with_line_number(true)
        .with_thread_ids(false)
        .with_target(false);

    let fmt_layer = tracing_subscriber::fmt::layer()
        .event_format(format)
        .with_span_events(FmtSpan::ACTIVE);

    let filter_layer = EnvFilter::try_from_env(LUMEC_LOG_ENV)
        .or_else(|_| {
            EnvFilter::builder()
                .with_default_directive(LevelFilter::ERROR.into())
                .parse("info,lume_lexer=warn")
        })
        .unwrap();

    let tracer = tracing_subscriber::registry().with(filter_layer).with(fmt_layer);

    tracing::subscriber::set_global_default(tracer).unwrap();

    tracing_log::LogTracer::init().unwrap();
}
