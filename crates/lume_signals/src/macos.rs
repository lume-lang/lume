use std::alloc::Layout;

use libc::siginfo_t;

unsafe extern "C" {
    fn backtrace_symbols_fd(buffer: *const *mut libc::c_void, size: libc::c_int, fd: libc::c_int);
}

const SIGNALS: [(libc::c_int, &str); 3] = [
    (libc::SIGILL, "SIGILL"),
    (libc::SIGBUS, "SIGBUS"),
    (libc::SIGSEGV, "SIGSEGV"),
];

pub fn install_handlers() {
    // Install alternate stack for signal handlers
    unsafe {
        let stack_size: usize = libc::MINSIGSTKSZ + 64 * 1024;
        let stack_ptr = std::alloc::alloc(Layout::from_size_align(stack_size, 1).unwrap());

        let mut ss: libc::stack_t = std::mem::zeroed();
        ss.ss_sp = stack_ptr.cast();
        ss.ss_size = stack_size;

        libc::sigaltstack(&raw const ss, std::ptr::null_mut());
    }

    // Install all the signals which should be handled
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = signal_handler as libc::sighandler_t;
        sa.sa_flags = libc::SA_NODEFER | libc::SA_RESETHAND | libc::SA_ONSTACK | libc::SA_SIGINFO;
        libc::sigemptyset(&raw mut sa.sa_mask);

        for (sigint, _signame) in SIGNALS {
            libc::sigaction(sigint, &raw const sa, std::ptr::null_mut());
        }
    }
}

pub fn uninstall_handlers() {}

#[allow(clippy::disallowed_macros)]
unsafe extern "C" fn signal_handler(signum: libc::c_int, info: *const siginfo_t, _ucontext: *const libc::c_void) {
    let signame = SIGNALS
        .iter()
        .find_map(|(sigint, signame)| (*sigint == signum).then_some(*signame))
        .unwrap_or("<unknown>");

    let info = unsafe { info.read() };

    let stack = unsafe {
        const MAX_FRAMES: usize = 256;

        // Prevents allocation within the signal handler.
        static mut STACK_TRACE: [*mut libc::c_void; MAX_FRAMES] = [std::ptr::null_mut(); MAX_FRAMES];

        let depth = libc::backtrace((&raw mut STACK_TRACE).cast(), MAX_FRAMES as i32);
        if depth <= 0 {
            return;
        }

        #[allow(clippy::cast_sign_loss, reason = "infallible")]
        std::slice::from_raw_parts((&raw const STACK_TRACE).cast(), depth as _)
    };

    #[cfg(feature = "color")]
    {
        use owo_colors::OwoColorize;
        use owo_colors::Stream::Stderr;

        eprintln!(
            "{}: lume application interrupted by {} (reason = {}, address = {:p})\n",
            "error".if_supports_color(Stderr, |t| t.red()),
            signame.if_supports_color(Stderr, |t| t.yellow()),
            sigerrno(info).if_supports_color(Stderr, |t| t.yellow()),
            info.si_addr.if_supports_color(Stderr, |t| t.red()),
        );
    }

    #[cfg(not(feature = "color"))]
    eprintln!(
        "error: lume application interrupted by {signame} (reason = {}, address = {:p})\n",
        sigerrno(info),
        info.si_addr
    );

    let size = stack.len().try_into().unwrap_or_default();
    unsafe {
        backtrace_symbols_fd(stack.as_ptr(), size, libc::STDERR_FILENO);
    }

    eprintln!();

    unsafe { libc::_exit(128 + signum) };
}

fn sigerrno(info: siginfo_t) -> &'static str {
    match info.si_signo {
        libc::SIGILL => match info.si_code {
            1 => "illegal opcode",          /* ILL_ILLOPC */
            2 => "illegal trap",            /* ILL_ILLTRP */
            3 => "privileged instruction",  /* ILL_PRVOPC */
            4 => "illegal operand",         /* ILL_ILLOPN */
            5 => "illegal addressing mode", /* ILL_ILLADR */
            6 => "privileged register",     /* ILL_PRVREG */
            7 => "coprocessor error",       /* ILL_COPROC */
            8 => "internal stack error",    /* ILL_BADSTK */
            _ => "unknown",
        },
        libc::SIGFPE => match info.si_code {
            1 => "floating-point divide by zero",    /* FPE_FLTDIV */
            2 => "floating-point overflow",          /* FPE_FLTOVF */
            3 => "floating-point underflow",         /* FPE_FLTUND */
            4 => "floating-point inexact result",    /* FPE_FLTRES */
            5 => "invalid floating-point operation", /* FPE_FLTINV */
            6 => "subscript out of range",           /* FPE_FLTSUB */
            7 => "integer divide by zero",           /* FPE_FLTDIV */
            8 => "integer overflow",                 /* FPE_FLTOVF */
            _ => "unknown",
        },
        libc::SIGSEGV => match info.si_code {
            1 => "address not mapped",    /* SEGV_MAPERR */
            2 => "address not available", /* SEGV_ACCERR */
            _ => "unknown",
        },
        libc::SIGBUS => match info.si_code {
            1 => "address not mapped",             /* BUS_ADRALN */
            2 => "on-existent physical address",   /* BUS_ADRERR */
            3 => "object-specific hardware error", /* BUS_OBJERR */
            _ => "unknown",
        },
        libc::SIGTRAP => match info.si_code {
            1 => "breakpoint", /* TRAP_BRKPT */
            2 => "trace trap", /* TRAP_TRACE */
            _ => "unknown",
        },
        libc::SIGCHLD => match info.si_code {
            1 => "child has exited",                    /* CLD_EXITED */
            2 => "terminated abnormally, no core file", /* CLD_KILLED */
            3 => "terminated abnormally, core file",    /* CLD_DUMPED */
            4 => "traced child has trapped",            /* CLD_TRAPPED */
            5 => "child has stopped",                   /* CLD_STOPPED */
            6 => "stopped child has continued",         /* CLD_CONTINUED */
            _ => "unknown",
        },
        _ => "unknown",
    }
}
