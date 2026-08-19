use lume_errors::{DiagCtx, Severity, diagnostic};

#[test]
fn create_transaction_and_commit() {
    let dcx = DiagCtx::new();

    let mut transaction = dcx.begin_transaction();
    transaction.emit(diagnostic!("error").with_severity(Severity::Error));

    assert!(!dcx.is_tainted(), "parent context should be untainted");
    transaction.commit();

    assert!(dcx.is_tainted(), "parent context should be tainted");
}

#[test]
fn create_transaction_and_rollback() {
    let dcx = DiagCtx::new();

    let mut transaction = dcx.begin_transaction();
    transaction.emit(diagnostic!("error").with_severity(Severity::Error));

    assert!(!dcx.is_tainted(), "parent context should be untainted");
    transaction.rollback();

    assert!(!dcx.is_tainted(), "parent context should be untainted");
}

#[test]
fn subtransactions_commit_to_parent() {
    let dcx = DiagCtx::new();

    let mut transaction = dcx.begin_transaction();
    let mut subtransaction = transaction.begin_transaction();

    subtransaction.emit(diagnostic!("error").with_severity(Severity::Error));

    assert!(!dcx.is_tainted(), "parent context should be untainted");
    assert!(!transaction.is_tainted(), "parent transaction should be untainted");
    assert!(subtransaction.is_tainted(), "subtransaction should be tainted");

    subtransaction.commit();
    drop(subtransaction);

    assert!(!dcx.is_tainted(), "parent context should be untainted");
    assert!(transaction.is_tainted(), "parent transaction should be tainted");

    transaction.commit();
    assert!(dcx.is_tainted(), "parent context should be tainted");
}

#[test]
fn subtransactions_rollback_to_parent() {
    let dcx = DiagCtx::new();

    let mut transaction = dcx.begin_transaction();
    let mut subtransaction = transaction.begin_transaction();

    subtransaction.emit(diagnostic!("error").with_severity(Severity::Error));

    assert!(!dcx.is_tainted(), "parent context should be untainted");
    assert!(!transaction.is_tainted(), "parent transaction should be untainted");
    assert!(subtransaction.is_tainted(), "subtransaction should be tainted");

    subtransaction.rollback();
    drop(subtransaction);

    assert!(!dcx.is_tainted(), "parent context should be untainted");
    assert!(!transaction.is_tainted(), "parent transaction should be tainted");

    transaction.commit();
    assert!(!dcx.is_tainted(), "parent context should be tainted");
}

#[test]
fn transaction_commit_on_drop() {
    let dcx = DiagCtx::new();

    let transaction = dcx.begin_transaction_with(lume_errors::OnSuccess::Commit, lume_errors::OnFailure::Rollback);
    transaction.emit(diagnostic!("note").with_severity(Severity::Note));
    assert_eq!(dcx.len(), 0);

    drop(transaction);

    assert_eq!(dcx.len(), 1, "note should have been commited");
    assert!(!dcx.is_tainted());
}

#[test]
fn transaction_rollback_on_drop() {
    let dcx = DiagCtx::new();

    let transaction = dcx.begin_transaction_with(lume_errors::OnSuccess::Commit, lume_errors::OnFailure::Rollback);
    transaction.emit(diagnostic!("error").with_severity(Severity::Error));
    assert_eq!(dcx.len(), 0);

    drop(transaction);

    assert_eq!(dcx.len(), 0, "error shouldve been rolled back");
    assert!(!dcx.is_tainted(), "error shouldve been rolled back");
}
