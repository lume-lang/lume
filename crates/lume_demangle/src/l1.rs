use lume_errors::{Result, SimpleDiagnostic};

/// Prefix for all mangled names, indicating that the name is mangled, along
/// with the current version being used for the mangling.
pub const MANGLED_PREFIX: &str = "_L1";

pub fn try_demangle(mangled: &str) -> Result<String> {
    let mut demangler = Demangler {
        src: mangled,
        chars: mangled.chars().peekable(),
        idx: 0,
    };

    // If the given string isn't mangled, we have nothing to demangle.
    if !demangler.check_str(MANGLED_PREFIX) {
        return Ok(mangled.to_string());
    }

    if demangler.check_str("_Cf") {
        demangler.parse_function()
    } else if demangler.check_str("_Ci") {
        demangler.parse_impl()
    } else if demangler.check_str("_Ct") {
        demangler.parse_trait_method_def()
    } else if demangler.check_str("_Cs") {
        demangler.parse_trait_method_impl()
    } else if demangler.check_str("_S") {
        demangler.parse_struct_def()
    } else if demangler.check_str("_E") {
        demangler.parse_enum_def()
    } else if demangler.check_str("_Tt") {
        demangler.parse_trait_def()
    } else {
        return Err(SimpleDiagnostic::new("invalid symbol identifier").into());
    }
}

struct Demangler<'a> {
    src: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    idx: usize,
}

impl Demangler<'_> {
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn peek_str(&self, exp: &str) -> bool {
        self.src[self.idx..].starts_with(exp)
    }

    fn check_str(&mut self, exp: &str) -> bool {
        if self.peek_str(exp) {
            self.skip(exp.len());

            true
        } else {
            false
        }
    }

    fn skip(&mut self, count: usize) {
        self.idx += count;

        for _ in 0..count {
            self.chars.next();
        }
    }
}

impl Demangler<'_> {
    fn parse_identifier(&mut self) -> Result<String> {
        let start = self.idx;

        while let Some(c) = self.peek()
            && c.is_ascii_digit()
        {
            self.skip(1);
        }

        if start == self.idx {
            return Err(SimpleDiagnostic::new("expected length-prefixed identifier").into());
        }

        let len = self.src[start..self.idx].parse::<usize>().unwrap();
        let str = self.src[self.idx..self.idx + len].to_string();

        self.skip(len);

        Ok(str)
    }

    fn parse_path_name(&mut self) -> Result<String> {
        let mut path = Vec::new();

        while let Some(c) = self.peek()
            && c.is_ascii_digit()
        {
            path.push(self.parse_identifier()?);
        }

        Ok(path.join("::"))
    }
}

impl Demangler<'_> {
    fn parse_package_segment(&mut self) -> Result<String> {
        if !self.check_str("P") {
            return Err(SimpleDiagnostic::new("expected package segment").into());
        }

        self.parse_identifier()
    }

    fn parse_path_segment(&mut self) -> Result<String> {
        if !self.check_str("N") {
            return Err(SimpleDiagnostic::new("expected path segment").into());
        }

        self.parse_path_name()
    }

    fn parse_name_segment(&mut self) -> Result<String> {
        if !self.check_str("M") {
            return Err(SimpleDiagnostic::new("expected name segment").into());
        }

        self.parse_identifier()
    }

    fn parse_impl_segment(&mut self) -> Result<String> {
        if !self.check_str("I") {
            return Err(SimpleDiagnostic::new("expected implementor segment").into());
        }

        // Check if the implementor is a type argument
        if self.check_str("Y") {
            let type_param_name = self.parse_identifier()?;
            let mut constraints = Vec::new();

            while self.check_str("c") {
                constraints.push(self.parse_path_name()?);
            }

            if constraints.is_empty() {
                return Ok(format!("<{type_param_name}>"));
            } else {
                return Ok(format!("<{type_param_name}: {}>", constraints.join(" + ")));
            }
        }

        self.parse_path_name()
    }
}

impl Demangler<'_> {
    fn parse_function(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let func_name = self.parse_path_segment()?;

        Ok(format!("{package_name}::{func_name}"))
    }

    fn parse_impl(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let implementor = self.parse_impl_segment()?;
        let method_name = self.parse_name_segment()?;

        Ok(format!("{package_name}::{implementor}::{method_name}"))
    }

    fn parse_trait_method_def(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let method_name = self.parse_path_segment()?;

        Ok(format!("{package_name}::{method_name}"))
    }

    fn parse_trait_method_impl(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let trait_def_name = self.parse_path_segment()?;
        let method_def_name = self.parse_name_segment()?;
        let target_name = self.parse_impl_segment()?;

        Ok(format!(
            "{package_name}::<{target_name} as {trait_def_name}>::{method_def_name}"
        ))
    }

    fn parse_struct_def(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let struct_name = self.parse_path_segment()?;

        Ok(format!("{package_name}::{struct_name}"))
    }

    fn parse_enum_def(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let enum_name = self.parse_path_segment()?;

        Ok(format!("{package_name}::{enum_name}"))
    }

    fn parse_trait_def(&mut self) -> Result<String> {
        let package_name = self.parse_package_segment()?;
        let trait_name = self.parse_path_segment()?;

        Ok(format!("{package_name}::{trait_name}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unmangled_name() -> Result<()> {
        let mangled = "main";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "main");

        Ok(())
    }

    #[test]
    fn test_demangle_func_name() -> Result<()> {
        let mangled = "_L1_CfP10playgroundN4main";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::main");

        Ok(())
    }

    #[test]
    fn test_demangle_method_name() -> Result<()> {
        let mangled = "_L1_CiP10playgroundI3std4TypeM9is_scalar";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::std::Type::is_scalar");

        Ok(())
    }

    #[test]
    fn test_demangle_blanket_method_name() -> Result<()> {
        let mangled = "_L1_CiP10playgroundIY1TM3bar";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<T>::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_constrained_blanket_method_name() -> Result<()> {
        let mangled = "_L1_CiP10playgroundIY1Tc3FooM3bar";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<T: Foo>::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_more_constrained_blanket_method_name() -> Result<()> {
        let mangled = "_L1_CiP10playgroundIY1Tc3Fooc3BarM3baz";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<T: Foo + Bar>::baz");

        Ok(())
    }

    #[test]
    fn test_demangle_trait_method_def() -> Result<()> {
        let mangled = "_L1_CtP10playgroundN3Foo3bar";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::Foo::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_trait_method_impl() -> Result<()> {
        let mangled = "_L1_CsP10playgroundN3FooM3barI3std5Int32";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<std::Int32 as Foo>::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_trait_method_blanket_impl() -> Result<()> {
        let mangled = "_L1_CsP10playgroundN3FooM3barIY1T";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<<T> as Foo>::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_trait_method_constrained_blanket_impl() -> Result<()> {
        let mangled = "_L1_CsP10playgroundN3FooM3barIY1Tc1A";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<<T: A> as Foo>::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_trait_method_more_constrained_blanket_impl() -> Result<()> {
        let mangled = "_L1_CsP10playgroundN3FooM3barIY1Tc1Ac1B";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::<<T: A + B> as Foo>::bar");

        Ok(())
    }

    #[test]
    fn test_demangle_struct_name() -> Result<()> {
        let mangled = "_L1_SP10playgroundN3Foo";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::Foo");

        Ok(())
    }

    #[test]
    fn test_demangle_enum_name() -> Result<()> {
        let mangled = "_L1_EP10playgroundN3Foo";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::Foo");

        Ok(())
    }

    #[test]
    fn test_demangle_trait_name() -> Result<()> {
        let mangled = "_L1_TtP10playgroundN3Foo";
        let demangled = try_demangle(mangled)?;

        assert_eq!(demangled.as_str(), "playground::Foo");

        Ok(())
    }
}
