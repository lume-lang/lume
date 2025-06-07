use super::*;
use error_snippet::Error;

#[track_caller]
fn parser(input: &str) -> PackageParser {
    let path = Path::new("<test>");
    let source = SourceFile::internal(input.to_string());
    let dcx = DiagCtxHandle::shim();

    PackageParser::from_source(path, Arc::new(source), dcx)
}

#[track_caller]
fn parse(input: &str) -> Manifest {
    parser(input).parse().unwrap()
}

#[track_caller]
fn parse_err(input: &str) -> Error {
    parser(input).parse().unwrap_err()
}

macro_rules! assert_snap {
    ($input: expr) => {
        insta::assert_debug_snapshot!($input);
    };
}

macro_rules! assert_snap_eq {
    ($input: expr) => {
        let parsed = parse($input);

        insta::with_settings!({
            description => $input,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(parsed);
        });
    };
}

macro_rules! assert_err_snap_eq {
    ($input: expr) => {
        let parsed = parse_err($input);

        insta::with_settings!({
            description => $input,
            omit_expression => true
        }, {
            insta::assert_debug_snapshot!(parsed);
        });
    };
}

#[test]
fn test_empty_file() {
    assert_err_snap_eq!("");
}

#[test]
fn test_without_name() {
    assert_err_snap_eq!("Package {}");
}

#[test]
fn test_without_lume_version() {
    assert_err_snap_eq!("Package \"sample\" {}");
}

#[test]
fn test_with_unexpected_name_type() {
    assert_err_snap_eq!("Package 123 {}");
}

#[test]
fn test_with_unexpected_lume_version_type() {
    assert_err_snap_eq!(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = 1
        }"
    );
}

#[test]
fn test_invalid_version_string() {
    assert_err_snap_eq!(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"^1-1\"
        }"
    );
}

#[test]
fn test_name() {
    assert_snap_eq!(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"^0\"
        }"
    );
}

#[test]
fn test_description() {
    assert_snap_eq!(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"^0\"
            description = \"Some description\"
        }"
    );
}

#[test]
fn test_license() {
    assert_snap_eq!(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"^0\"
            license = \"MIT\"
        }"
    );
}

#[test]
fn test_repository() {
    assert_snap_eq!(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"^0\"
            repository = \"http://github.com/lume-lang/lume\"
        }"
    );
}

#[test]
fn test_version() {
    assert_snap_eq!(
        "Package \"sample\" {
            lume_version = \"^0\"
            version = \"1.0.0\"
        }"
    );
}

#[test]
fn test_incompatible_version() {
    let mut parser = parser(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"^2\"
        }",
    );

    parser.current_lume_version = Version::new(1, 0, 0);

    assert_snap!(parser.parse().unwrap_err());
}

#[test]
fn test_prerelease_lume_version_success() {
    let mut parser = parser(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"1.0.0-rc3\"
        }",
    );

    parser.current_lume_version = Version::parse("1.0.0-rc3").unwrap();

    assert_snap!(parser.parse().unwrap());
}

#[test]
fn test_prerelease_lume_version_stable() {
    let mut parser = parser(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"1.0.0-rc3\"
        }",
    );

    parser.current_lume_version = Version::parse("1.0.0").unwrap();

    assert_snap!(parser.parse().unwrap());
}

#[test]
fn test_prerelease_lume_version_failure() {
    let mut parser = parser(
        "Package \"sample\" {
            version = \"1.0.0\"
            lume_version = \"1.0.0-rc3\"
        }",
    );

    parser.current_lume_version = Version::parse("1.0.0-rc2").unwrap();

    assert_snap!(parser.parse().unwrap_err());
}

#[test]
fn test_multiple_packages() {
    assert_err_snap_eq!(
        "Package \"package01\" { version = \"1.0.0\" lume_version = \"^0\" }
        Package \"package02\" { version = \"1.0.0\" lume_version = \"^0\" }
        Package \"package03\" { version = \"1.0.0\" lume_version = \"^0\" }"
    );
}
