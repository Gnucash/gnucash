# QIF date-format reproductions

These small, synthetic QIF files exercise date detection, the candidate list supplied to the import assistant, and reparsing after a selection. They contain no personal financial data. The automated tests read these exact files through `qif-file:read-file` and use the real GnuCash modules.

## Verified baseline

Tested against upstream stable commit `3c9cfbb91` with Guile 3.0.11. The production importer is unchanged. The QIF library and Scheme test targets build successfully. The existing `test-qif-imp`, `test-qif-parse`, and `test-qif-merge-groups` suites pass. With the six additional multi-record ISO fixtures, the new `test-qif-date-formats` suite reports **105 passing assertions and 30 failing assertions**. CTest counts this as one failing suite, not 30 separate tests. These are ordinary failures demonstrating current defects, not expected-failure annotations.

The verification runs the reader, detector, field parser, candidate extractor, and reparser in the actual application modules. It does not drive GTK or complete an import into a GnuCash book. References below to picker choices mean the exact candidates returned at the Scheme/C boundary; the assistant clears its combo box and inserts those candidates at `assistant-qif-import.c:2052–2076`.

## Files and results

| File | Date text and separator | Current result |
|---|---|---|
| `ymd-slash.qif` | `2024/01/02`, slash | Picker candidates are YMD/YDM. Selecting YMD succeeds; selecting YDM fails. |
| `ymd-hyphen.qif` | `2024-01-02`, hyphen | Same as slash. |
| `ymd-dot.qif` | `2024.01.02`, dot | Same as slash. |
| `ymd-apostrophe.qif` | `2024'01'02`, ASCII apostrophe | Same as slash. |
| `ymd-compact.qif` | `20240102`, no separator | Picker candidates are YMD/YDM, but selecting either raises a Scheme type error. |
| `ymd-day13.qif` | `2024/01/13`, slash | Successful control: no date selection needed; January 13 is parsed. |
| `ydm-day13.qif` | `2024/13/01`, slash | Unambiguous YDM is detected but parsing fails. |
| `year-first-equal-parts.qif` | `2024/03/03`, slash | Unnecessary YMD/YDM ambiguity warning; either format should mean March 3. |
| `mixed-price-ymd-transaction-mdy.qif` | Price `2024/01/02`; transaction `02/03/2024`; both slash | Both groups are ambiguous. The picker receives only MDY/DMY. Selecting MDY parses the transaction but leaves the price date as a string. |
| `price-and-transaction-ymd.qif` | Price and transaction both `2024/01/02`, slash | Picker receives YMD/YDM. Selecting YMD parses the transaction but leaves the price date as a string. |
| `price-ymd-transaction-unambiguous.qif` | Price `2024/01/02`; transaction `01/13/2024`; both slash | Only the price is ambiguous. Picker receives YMD/YDM, but selecting YMD returns false and leaves the price unparsed. The already-resolved transaction survives. |

The transaction-only samples with delimiters do **not** reproduce year-first dates being offered year-last choices. The mixed price/transaction sample does reproduce that candidate-list symptom. It establishes a specific failing path, not that every year-first import has this problem or that this synthetic input is identical to the original reported file.

## Reproduce through the assistant

### Additional multi-record ISO files

All six files below use literal hyphens and four-digit years (`YYYY-MM-DD`) for every date. Each contains four complete bank transactions with amounts, payees, categories, and record terminators. The two price fixtures also contain a security definition and four price records. All dates are valid calendar dates; the failing cases exercise interpretation and resolution rather than malformed input.

| File | Purpose | Verified backend result |
|---|---|---|
| `iso-multi-day31-first.qif` | `2022-12-31` first, followed by three dates with days ≤12 | Pass: all four transactions parse automatically as YMD. |
| `iso-multi-day31-last.qif` | The same transactions with `2022-12-31` moved to the end | Pass: detection examines later records and resolves all four automatically. |
| `iso-multi-days-at-most12.qif` | `2022-12-11`, `2023-01-02`, `2023-02-03`, `2024-04-05`; no day above 12 | Pass after selection: YMD/YDM are offered, and choosing YMD parses all four transactions correctly. |
| `iso-multi-equal-month-day.qif` | `2022-12-12`, `2023-01-01`, `2023-02-02`, `2024-03-03`; no day above 12 and no actual difference between YMD/YDM interpretations | Fails automatic resolution: an unnecessary YMD/YDM warning leaves all four dates raw. Two assertions fail. |
| `iso-multi-prices-ambiguous.qif` | Four transactions and four prices, using the same dates as `iso-multi-days-at-most12.qif` | Fails price resolution: choosing YMD parses all four transactions but leaves all four price dates raw. One assertion fails. |
| `iso-multi-prices-day31.qif` | Four transactions and four prices, each group including `2022-12-31` | Pass: both groups resolve automatically, including every price. |

The day-31-last fixtures are intentionally not in chronological order, so they test record-order handling rather than assuming exports are sorted. The price reader stores price records in reverse file order; the assertions account for that while checking every resulting date. These six fixtures add 39 passing and three failing assertions to the original 66 passing and 27 failing assertions.

For manual comparison, start with `iso-multi-prices-day31.qif` and `iso-multi-prices-ambiguous.qif`: the former resolves all dates without a picker; the latter has only days ≤12 and exposes the unresolved-price problem after selecting YMD. The transaction-only `iso-multi-days-at-most12.qif` is the matching successful selection control.

### Manual steps

1. Start GnuCash with a new book and open **File → Import → Import QIF**.
2. Load one sample file at a time and start its read/parse step.
3. For `mixed-price-ymd-transaction-mdy.qif`, inspect the date-format page. The backend supplies only `m-d-y` and `d-m-y`, despite the unresolved year-first price date. Select `m-d-y`.
4. Compare with `ymd-slash.qif`, for which the backend supplies `y-m-d` and `y-d-m`; selecting `y-m-d` resolves the transaction successfully.
5. Try `ymd-compact.qif` and select `y-m-d`. This exercises the no-separator failure after correct initial detection. Record the assistant's visible behavior and terminal output; the automated test catches the Scheme error but does not establish how the running GUI presents it.
6. Use the other samples to isolate the YDM conversion and price-date resolution failures listed above.

The tests establish the unresolved internal date values immediately after the callback's Scheme operation. They do not claim a particular subsequent GUI error message or final imported price. No final import is necessary to compare the candidate lists.

## Build and run the regression tests

From the source root, with the GnuCash GUI/Guile development dependencies installed:

```sh
cmake -S . -B ../cashfix-build \
  -D WITH_GNUCASH=ON \
  -D WITH_AQBANKING=OFF \
  -D WITH_OFX=OFF \
  -D WITH_SQL=OFF \
  -D WITH_PYTHON=OFF
cmake --build ../cashfix-build \
  --target gnc-qif-import scm-test-qif-imp-srfi64 -j 4
ctest --test-dir ../cashfix-build --output-on-failure \
  -R 'test-qif-(imp|parse|merge-groups|date-formats)'
```

The last command is supposed to exit nonzero on the baseline because the new suite demonstrates unresolved bugs. To run just the new suite with complete output:

```sh
ctest --test-dir ../cashfix-build -V -R '^test-qif-date-formats$'
```

CMake supplies `QIF_DATE_FIXTURES` so the test reads these exact source-tree files. It also sets the build-tree runtime paths needed to load the XML backend. No graphical display is required for these tests, although the Scheme modules depend on compiled GUI libraries.

To build the executable for the manual assistant checks:

```sh
cmake --build ../cashfix-build --target gnucash -j 4
../cashfix-build/bin/gnucash
```

The executable/manual checks above are instructions for further verification; the verified build in this investigation targeted the QIF library and Scheme tests.

## What the failures isolate

### 1. The year-last picker result is a warning collision

For `mixed-price-ymd-transaction-mdy.qif`, the real `qif-file:parse-fields` result is:

```scheme
(#t (date m-d-y d-m-y) (date y-m-d y-d-m))
```

The assistant's extraction:

```scheme
(qif-file:parse-fields-results (cdr results) 'date)
```

returns:

```scheme
(m-d-y d-m-y)
```

Price and transaction dates are checked separately, but both warnings have the `date` key. The transaction warning is first because it is added later and warnings are prepended. No candidate list is converted from year-first to year-last; the first warning hides the second. The tests include passing diagnostic assertions for this exact result, followed by a failing assertion that date-page completion must not leave unresolved price dates. Those diagnostic snapshots may need updating if the result protocol changes.

`qif-file:reparse-dates` currently visits transactions only. Returning the price warning first would therefore not be a complete fix. `price-and-transaction-ymd.qif` demonstrates the unresolved-price failure without requiring different format choices for prices and transactions. The assistant currently ignores the reparse result and completes the page.

### 2. Compact dates violate the restricted-candidate contract

Initial detection of `20240102` with all four formats correctly returns `(y-m-d y-d-m)`. But after choosing YMD, the reparser calls the detector with only `(y-m-d)`. The result is an improper list:

```scheme
(qif-parse:check-date-format "20240102" '(y-m-d))
;; actual:   (y-m-d . #<unspecified>)
;; expected: (y-m-d)
```

In the compact-date branch of `qif-parse:check-date-format`, two `if` expressions are passed to `append` without an else value. Excluding one format family supplies an unspecified value instead of an empty list. The reparse then raises `Wrong type argument: #<unspecified>`. The singleton-candidate assertions isolate this defect independently of the YDM conversion defect.

### 3. YDM detection succeeds but conversion uses the wrong positions

The YDM branch of `qif-parse:parse-date/format` calls `(refs->list 2 0 1)`. The helper expects day/month/year positions; for year/day/month input those positions should be `1 2 0`. The current code treats the year as the month. This fails actual YDM conversion and also prevents the equivalence check from recognizing that `2024/03/03` has only one interpretation.

### Assertion counts by cause

| Group | Failing assertions |
|---|---:|
| YDM conversion/selection across four delimited samples | 12 |
| Compact sample: singleton detection, selection, conversion and final dates | 7 |
| Unambiguous YDM and equal-component samples | 4 |
| Price-date selection across three samples | 4 |
| Multi-record ISO samples: equal month/day and unresolved prices | 3 |
| Total | 30 |

These assertions retain support for all four existing format candidates. They do not assume that YMD should be preferred over YDM merely because the year comes first.

## Suggested issue response

> I have attached minimal synthetic QIF files and added tests that read them through the actual importer modules on stable `3c9cfbb91`. The mixed price/transaction reproduction uses `/`: the price date is `2024/01/02` and the transaction date is `02/03/2024`. `parse-fields` returns `(#t (date m-d-y d-m-y) (date y-m-d y-d-m))`, and the extraction used by the assistant returns only `(m-d-y d-m-y)`. After choosing MDY, the transaction is parsed but the price date remains a string.
>
> In contrast, a transaction-only `2024/01/02` sample correctly supplies the year-first choices and resolves when YMD is selected. So the tests narrow down a specific path rather than showing a universal failure of year-first detection.
>
> There is also a separate no-separator reproduction, `20240102`: initial detection works, but the singleton-candidate check used after selecting YMD returns `(y-m-d . #<unspecified>)`, causing a type error during reparsing. The new tests capture that separately, along with the YDM component-order and price-reparsing problems. Six additional multi-record `YYYY-MM-DD` samples cover automatic resolution, selection when all days are at most 12, and price-date resolution. The three existing QIF test suites pass; the new regression suite has 105 passing and 30 failing assertions against the unchanged importer.

This draft describes the supplied synthetic files. The separator used in the original reported export should be confirmed separately rather than inferred from these samples.
