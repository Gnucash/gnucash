# GTK3 preparation for the GTK4 review

This series remains on GTK3 and is based on `future` at
`2765929b8143f6af07a4530a8de6b674e62898e8`. It prepares response-driven dialog
control flow before resuming GTK4 graphics and then GApplication work. It does
not introduce GTK4 widgets or application ownership as a prerequisite to
reviewing the dialogs.

## How to review the series

The transfer-constructor extraction is one independent preparation commit.
The WebView2 include classification is a separate Windows build change.
Dialog changes are divided by subsystem below, with their tests alongside
the product changes. Shared test registration and CI display requirements are
collected at the end.

**The dialog commits are review intermediates.** Removing synchronous APIs and
updating their consumers crosses subsystem boundaries, so intermediate
commits are not claimed to build or pass the test suite. The complete endpoint
is validated. Before merge, the coupled dialog intermediates must be squashed
to preserve usable bisects; the independent transfer preparation should stay
separate. This follows the review-series option discussed in PR #2319.

These tests exercise the new asynchronous contracts. They are not presented
as a separate collection of tests that already pass unchanged on `future`.

## Commit groups and contracts

| Group | Reason and important files | Relevant validation |
| --- | --- | --- |
| Transfer constructor | Move transaction construction out of `dialog-transfer.cpp` into `Transaction.cpp` as `gnc_transaction_from_transaction_info`, declared in `Transaction.h`. Preserve book ownership, account/commodity amounts and the book's number/action setting. Keep borrowed input structures out of SWIG. | `test-transfer-transaction` |
| Windows SDK includes | Treat WebView2 include directories as external SDK headers in `gnucash/html/CMakeLists.txt`. This is independent of the dialog migration. | Windows product build |
| Shared dialog contracts | `dialog-utils`, `gnc-gui-query`, user/password helpers and notice dialogs deliver results through callbacks. Request data owns strings and references; closing a parent is a terminal cancellation. `object_references_response_cb` uses `[[maybe_unused]]` for its unused parameters. | GUI-query, input-lifetime, object-reference, date-range and preferences tests |
| Accounts and commodities | Account, commodity, tax and general selectors return through callbacks rather than assuming a stack-local result. A late selection cannot edit a closed account or book. | Account cascade/children, commodity, tax and general-selection tests |
| Register and ledger | Deferred confirmations resume the requested edit only after an affirmative response. Capture stable identities and release edit/refresh state before awaiting input. Transfer UI uses the independent engine constructor. | Entry-ledger close, readonly threshold, reconciliation-cell and deferred-table tests |
| Business workflows | Invoice/order completion, posting, payments, bill terms, date and check dialogs continue in response order. Invoice currency prompts are sequential; cancellation does not post a partly completed invoice. | Invoice posting/deletion, payment, billterms, date-close and check-title tests |
| Budget, reports and scheduled transactions | Budget selection and report/SX operations capture their inputs before returning to the event loop. Closing pages or books prevents late callbacks from mutating their old state. | Budget selection/model, style-sheet, new-user and SX creation tests |
| File/session continuations | `gnc-file`, main-window closing, autosave and startup finish from the appropriate dialog response. Keep the displayed book/session valid through Save As and release request ownership exactly once. Reject overlapping file commands. | File chooser/error/save/open, main-window close, autosave, doclink and encoding tests |
| Generic import | Account/security pickers and the matcher own pending requests and revalidate their book. Prepare queued transaction rows before exposing the matcher; install completion before a response can occur. Disconnect signals before freeing the matcher. | Import-account matcher and real generic-matcher response tests |
| Import assistants | CSV, QIF, business/customer import and log replay resume their existing operation after input rather than reading a synchronous result. Preserve the existing selection and import semantics. | CSV/business/customer regex response tests and product build |
| OFX import | First parse collects account/security requirements; asynchronous choices resolve them; the second parse creates transactions. Retain the import until matcher/reconcile completion and cancel on parent/book closure. | Real LibOFX fixture: populated matcher, accept, cancel and parent destruction |
| Gwen dialog bridge | Marshal backend-worker requests to the GTK thread. Only a worker waits for its result. Close GTK widgets while the Gwen dialog and guards still exist, before invoking completion. | Five real Gwen async-dialog scenarios and worker tests |
| AqBanking context | Traverse account transactions and balances in response order; the matcher stays hidden until its rows and completion are ready. Hold operation ownership through the complete import. | Real AqBanking context: accept, cancel and parent destruction |
| AqBanking frontends | Get-balance, get-transactions, file import, setup and transfers retain their context, Gwen reservation, operation slot and session lease until the terminal callback. A later matcher cancellation cannot undo a bank-accepted payment. | AqBanking module build, existing AqBanking tests, worker/context/dialog tests |
| Registration and guide | Register the grouped tests, require a display for display-dependent cases, and provide accessibility services in the Linux GTK3 test environment. Keep new tests in test directories. | Final endpoint test run |

## Scope of file/session and import changes

The file path has no file-transition queue and no background QOF loader.
Session loading stays on the existing thread after the necessary user
responses. Pending-operation guards prevent a second command or book closure
from invalidating the first operation's callback data. The session exchange
helper supports reversible Save As while maintaining the displayed-book
invariant; it is not an engine-concurrency migration.

Import request ownership is needed because account, commodity and matcher
answers now outlive their initiating call. It covers cancellation and retained
widget signals after parent destruction. AqBanking's operation slot covers
its shared backend and remains held through matcher completion; serializing
only the worker would release that backend too early.

## Validation and limits

The final GTK3 endpoint builds GnuCash, `gncmod-aqbanking` and `gncmod-ofx`
with banking and OFX enabled. All 50 selected GTK/dialog/banking test programs
pass without skipped cases, using native Windows binaries and a fresh GTK3
Broadway display per program. The endpoint has no project `gtk_dialog_run`
calls or remaining consumers of the removed synchronous project wrappers.

The local validation logs are `validation/final-gtk3-build.log` and
`validation/final-gtk3-runtime.log` in the task workspace, outside the source
checkout. They are not files shipped by GnuCash. The reviewed dialog product
and test sources are identical to that validated endpoint; only history and
this guide/distribution entry have changed.

These runs do not establish manual Win32, Linux/macOS or live-bank acceptance.
The OFX fixture covers bank transactions, not the complete investment/security
matrix. Gwen retains its original synchronous callback for nested third-party
dialogs invoked directly on the GTK thread; its worker bridge and our setup
dialog are migrated. No new project nested dialog loop is introduced.

Scheme-startup extraction, GTK4 graphics and GApplication integration remain
later review stages. Platform dependency and CI decisions for that GTK4 stage
are not claimed complete by this GTK3 preparation.
