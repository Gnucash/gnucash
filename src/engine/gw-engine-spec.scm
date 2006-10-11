(define-module (g-wrapped gw-engine-spec))
(debug-set! maxdepth 100000)
(debug-set! stack    200000)

(use-modules (g-wrap))
(use-modules (g-wrap simple-type))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

(define ws (gw:new-wrapset "gw-engine"))

(gw:wrapset-depends-on ws "gw-standard")
(gw:wrapset-depends-on ws "gw-wct")
(gw:wrapset-depends-on ws "gw-glib")

(gw:wrapset-set-guile-module! ws '(g-wrapped gw-engine))

(gw:wrapset-add-cs-declarations!
 ws
 (lambda (wrapset client-wrapset)
   (list
    "#include <config.h>\n"
    "#include <glib.h>\n"
    "#include <qof.h>\n"
    "#include <Group.h>\n"
    "#include <Query.h>\n"
    "#include <gnc-budget.h>\n"
    "#include <gnc-commodity.h>\n"
    "#include <gnc-engine.h>\n"
    "#include <gnc-filepath-utils.h>\n"
    "#include <gnc-pricedb.h>\n"
    "#include <gnc-lot.h>\n"
    "#include <gnc-session-scm.h>\n"
    "#include <gnc-hooks-scm.h>\n"
    "#include <engine-helpers.h>\n"
    "#include <SX-book.h>\n")))

(gw:wrapset-add-cs-initializers!
 ws
 (lambda (wrapset client-wrapset status-var) 
   (if client-wrapset
       '()
       (gw:inline-scheme '(use-modules (gnucash engine))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <gnc:time-pair>
(gw:wrap-simple-type ws
                     '<gnc:time-pair> "Timespec"
                     '("gnc_timepair_p(" scm-var ")")
                     '(c-var " = gnc_timepair2timespec(" scm-var ");\n")
                     '(scm-var " = gnc_timespec2timepair(" c-var ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <gnc:guid-scm>
(gw:wrap-simple-type ws
                     '<gnc:guid-scm> "GUID"
                     '("gnc_guid_p(" scm-var ")")
                     '(c-var " = gnc_scm2guid(" scm-var ");\n")
                     '(scm-var " = gnc_guid2scm(" c-var ");\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <gnc:numeric>
(gw:wrap-simple-type ws
                     '<gnc:numeric> "gnc_numeric"
                     '("gnc_numeric_p(" scm-var ")")
                     '(c-var " = gnc_scm_to_numeric(" scm-var ");\n")
                     '(scm-var " = gnc_numeric_to_scm(" c-var ");\n"))

;; Equivalencies
;;
;; <gnc:list-of-commodity*> -> (gw:glist-of <gnc:commodity*> caller-owned)
;; <gnc:list-of-price*> -> (gw:glist-of <gnc:Price*> caller-owned)
;;
;; <gnc:list-of-split*-caller-owned> -> (gw:glist-of <gnc:Split*> caller-owned)
;; <gnc:list-of-split*-callee-owned> -> (gw:glist-of <gnc:Split*> callee-owned)
;;
;; <gnc:list-of-transaction*-callee-owned> ->
;;   (gw:glist-of <gnc:Transaction*> callee-owned)
;; <gnc:list-of-transaction*-caller-owned> ->
;;   (gw:glist-of <gnc:Transaction*> caller-owned)
;;
;; <gnc:list-of-account*-callee-owned> ->
;;   (gw:glist-of <gnc:Account*> callee-owned)
;; <gnc:list-of-account*-caller-owned> ->
;;   (gw:glist-of <gnc:Account*> caller-owned)
;;
;; <gnc:list-of-string> -> '(gw:glist-of (<gw:mchars> callee-owned) callee-owned)


(gw:wrap-as-wct ws '<gnc:GList*> "GList*" "const GList*")

(gw:wrap-as-wct ws '<gnc:id-type> "QofIdType" "QofIdTypeConst")
(gw:wrap-as-wct ws '<gnc:Account*> "Account*" "const Account*")
(gw:wrap-as-wct ws '<gnc:Account**> "Account**" "const Account**")
(gw:wrap-as-wct ws '<gnc:AccountGroup*> "AccountGroup*" "const AccountGroup*")
(gw:wrap-as-wct ws '<gnc:Book*> "QofBook*" "const QofBook*")
(gw:wrap-as-wct ws '<gnc:Lot*> "GNCLot*" "const GNCLot*")
(gw:wrap-as-wct ws '<gnc:Session*> "QofSession*" "const QofSession**")
(gw:wrap-as-wct ws '<gnc:Split*> "Split*" "const Split*")
(gw:wrap-as-wct ws '<gnc:Transaction*> "Transaction*" "const Transaction*")  
(gw:wrap-as-wct ws '<gnc:commodity*> "gnc_commodity*" "const gnc_commodity*")
(gw:wrap-as-wct ws '<gnc:commodity-namespace*>
                "gnc_commodity_namespace*" 
                "const gnc_commodity_namespace*")
(gw:wrap-as-wct ws '<gnc:commodity-table*>
                "gnc_commodity_table*" 
                "const gnc_commodity_table*")

(gw:wrap-as-wct ws '<gnc:Query*> "Query *" "const Query *")

(let ((wt (gw:wrap-enumeration ws '<gnc:event-type> "QofEventId")))

  (gw:enum-add-value! wt "QOF_EVENT_NONE" 'qof-event-none)
  (gw:enum-add-value! wt "QOF_EVENT_CREATE" 'qof-event-create)
  (gw:enum-add-value! wt "QOF_EVENT_MODIFY" 'qof-event-modify)
  (gw:enum-add-value! wt "QOF_EVENT_DESTROY" 'qof-event-destroy)
  (gw:enum-add-value! wt "QOF_EVENT_ALL" 'qof-event-all))

(let ((wt (gw:wrap-enumeration ws '<gnc:query-op> "QofQueryOp")))

  (gw:enum-add-value! wt "QOF_QUERY_AND" 'query-and)
  (gw:enum-add-value! wt "QOF_QUERY_OR" 'query-or)
  (gw:enum-add-value! wt "QOF_QUERY_NAND" 'query-nand)
  (gw:enum-add-value! wt "QOF_QUERY_NOR" 'query-nor)
  (gw:enum-add-value! wt "QOF_QUERY_XOR" 'query-xor))

(let ((wt (gw:wrap-enumeration ws '<gnc:query-compare-how> "QofQueryCompare")))
  (gw:enum-add-value! wt "QOF_COMPARE_LT" 'query-compare-lt)
  (gw:enum-add-value! wt "QOF_COMPARE_LTE" 'query-compare-lte)
  (gw:enum-add-value! wt "QOF_COMPARE_EQUAL" 'query-compare-equal)
  (gw:enum-add-value! wt "QOF_COMPARE_GT" 'query-compare-gt)
  (gw:enum-add-value! wt "QOF_COMPARE_GTE" 'query-compare-gte)
  (gw:enum-add-value! wt "QOF_COMPARE_NEQ" 'query-compare-neq)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:string-match-how> "QofStringMatch")))
  (gw:enum-add-value! wt "QOF_STRING_MATCH_NORMAL" 'string-match-normal)
  (gw:enum-add-value! wt "QOF_STRING_MATCH_CASEINSENSITIVE" 'string-match-caseinsensitive)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:date-match-how> "QofDateMatch")))
  (gw:enum-add-value! wt "QOF_DATE_MATCH_NORMAL" 'date-match-normal)
  (gw:enum-add-value! wt "QOF_DATE_MATCH_DAY" 'date-match-rounded)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:numeric-match-how> "QofNumericMatch")))
  (gw:enum-add-value! wt "QOF_NUMERIC_MATCH_ANY" 'amt-sgn-match-either)
  (gw:enum-add-value! wt "QOF_NUMERIC_MATCH_CREDIT" 'amt-sgn-match-credit)
  (gw:enum-add-value! wt "QOF_NUMERIC_MATCH_DEBIT" 'amt-sgn-match-debit)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:cleared-match-how> "cleared_match_t")))
  (gw:enum-add-value! wt "CLEARED_NO" 'cleared-match-no)
  (gw:enum-add-value! wt "CLEARED_CLEARED" 'cleared-match-cleared)
  (gw:enum-add-value! wt "CLEARED_RECONCILED" 'cleared-match-reconciled)
  (gw:enum-add-value! wt "CLEARED_FROZEN" 'cleared-match-frozen)
  (gw:enum-add-value! wt "CLEARED_VOIDED" 'cleared-match-voided)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:guid-match-how> "QofGuidMatch")))
  (gw:enum-add-value! wt "QOF_GUID_MATCH_ALL" 'guid-match-all)
  (gw:enum-add-value! wt "QOF_GUID_MATCH_ANY" 'guid-match-any)
  (gw:enum-add-value! wt "QOF_GUID_MATCH_NULL" 'guid-match-null)
  (gw:enum-add-value! wt "QOF_GUID_MATCH_NONE" 'guid-match-none)
  (gw:enum-add-value! wt "QOF_GUID_MATCH_LIST_ANY" 'guid-match-list-any)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:char-match-how> "QofCharMatch")))
  (gw:enum-add-value! wt "QOF_CHAR_MATCH_ANY" 'char-match-any)
  (gw:enum-add-value! wt "QOF_CHAR_MATCH_NONE" 'char-match-none)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:query-txn-match-t> "query_txn_match_t")))
  (gw:enum-add-value! wt "QUERY_TXN_MATCH_ALL" 'query-txn-match-all)    
  (gw:enum-add-value! wt "QUERY_TXN_MATCH_ANY" 'query-txn-match-any)
  #t)

(let ((wt (gw:wrap-enumeration ws '<gnc:kvp-value-t> "KvpValueType")))
  (gw:enum-add-value! wt "KVP_TYPE_GINT64" 'kvp-type-gint64)
  (gw:enum-add-value! wt "KVP_TYPE_DOUBLE" 'kvp-type-double)
  (gw:enum-add-value! wt "KVP_TYPE_NUMERIC" 'kvp-type-numeric)
  (gw:enum-add-value! wt "KVP_TYPE_STRING" 'kvp-type-string)
  (gw:enum-add-value! wt "KVP_TYPE_GUID" 'kvp-type-guid)
  (gw:enum-add-value! wt "KVP_TYPE_TIMESPEC" 'kvp-type-timespec)
  (gw:enum-add-value! wt "KVP_TYPE_BINARY" 'kvp-type-binary)
  (gw:enum-add-value! wt "KVP_TYPE_GLIST" 'kvp-type-glist)
  (gw:enum-add-value! wt "KVP_TYPE_FRAME" 'kvp-type-frame)
  #t)

(let ((we (gw:wrap-enumeration ws '<gnc:AccountType> "GNCAccountType")))
  ;; From Account.h
  (gw:enum-add-value! we "ACCT_TYPE_INVALID" 'bad-type)
  (gw:enum-add-value! we "ACCT_TYPE_NONE" 'no-type)
  (gw:enum-add-value! we "ACCT_TYPE_BANK" 'bank)
  (gw:enum-add-value! we "ACCT_TYPE_CASH" 'cash)
  (gw:enum-add-value! we "ACCT_TYPE_CREDIT" 'credit)
  (gw:enum-add-value! we "ACCT_TYPE_ASSET" 'asset)
  (gw:enum-add-value! we "ACCT_TYPE_LIABILITY" 'liability)
  (gw:enum-add-value! we "ACCT_TYPE_STOCK" 'stock)
  (gw:enum-add-value! we "ACCT_TYPE_MUTUAL" 'mutual-fund)
  (gw:enum-add-value! we "ACCT_TYPE_CURRENCY" 'currency)
  (gw:enum-add-value! we "ACCT_TYPE_INCOME" 'income)
  (gw:enum-add-value! we "ACCT_TYPE_EXPENSE" 'expense)
  (gw:enum-add-value! we "ACCT_TYPE_EQUITY" 'equity)
  (gw:enum-add-value! we "ACCT_TYPE_RECEIVABLE" 'receivable)
  (gw:enum-add-value! we "ACCT_TYPE_PAYABLE" 'payable)
  (gw:enum-add-value! we "NUM_ACCOUNT_TYPES" 'num-account-types)
  (gw:enum-add-value! we "ACCT_TYPE_CHECKING" 'checking)
  (gw:enum-add-value! we "ACCT_TYPE_SAVINGS" 'savings)
  (gw:enum-add-value! we "ACCT_TYPE_MONEYMRKT" 'money-market)
  (gw:enum-add-value! we "ACCT_TYPE_CREDITLINE" 'credit-line)
  #t)

(let ((we (gw:wrap-enumeration ws '<gnc:BackendError> "QofBackendError")))

  (gw:enum-add-value! we "ERR_BACKEND_NO_ERR" 'no-err)
  (gw:enum-add-value! we "ERR_BACKEND_NO_BACKEND" 'no-backend)
  (gw:enum-add-value! we "ERR_BACKEND_BAD_URL" 'bad-url)
  (gw:enum-add-value! we "ERR_BACKEND_CANT_CONNECT" 'cant-connect)
  (gw:enum-add-value! we "ERR_BACKEND_CONN_LOST" 'connection-lost)
  (gw:enum-add-value! we "ERR_BACKEND_NO_SUCH_DB" 'no-such-db)
  (gw:enum-add-value! we "ERR_BACKEND_LOCKED" 'locked)
  (gw:enum-add-value! we "ERR_BACKEND_READONLY" 'read-only)
  (gw:enum-add-value! we "ERR_BACKEND_DATA_CORRUPT" 'data-corrupt)
  (gw:enum-add-value! we "ERR_BACKEND_SERVER_ERR" 'server-error)
  (gw:enum-add-value! we "ERR_BACKEND_ALLOC" 'alloc)
  (gw:enum-add-value! we "ERR_BACKEND_MISC" 'misc)
  
  ;;     (gw:enum-add-value! we "ERR_FILEIO_FILE_BAD_READ" 'file-bad-read)
  ;;     (gw:enum-add-value! we "ERR_FILEIO_FILE_EMPTY" 'file-empty)
  ;;     (gw:enum-add-value! we "ERR_FILEIO_FILE_LOCKERR" 'file-lockerr)
  ;;     (gw:enum-add-value! we "ERR_FILEIO_FILE_NOT_FOUND" 'file-not-found)
  ;;     (gw:enum-add-value! we "ERR_FILEIO_FILE_TOO_NEW" 'file-too-new)
  ;;     (gw:enum-add-value! we "ERR_FILEIO_FILE_TOO_OLD" 'file-too-old)
  
  ;;     (gw:enum-add-value! we "ERR_NETIO_SHORT_READ" 'netio-short-read)
  ;;     (gw:enum-add-value! we "ERR_NETIO_WRONG_CONTENT_TYPE"
  ;;                         'netio-wrong-content-type)
  ;;     (gw:enum-add-value! we "ERR_NETIO_NOT_GNCXML" 'netio-not-gncxml)
  
  ;;     (gw:enum-add-value! we "ERR_SQL_MISSING_DATA" 'sql-missing-data)

  #t)

;
; Definitions from gnc-engine.h
;
(gw:wrap-value ws 'gnc:id-account '<gnc:id-type> "GNC_ID_ACCOUNT")
(gw:wrap-value ws 'gnc:id-book '<gnc:id-type> "GNC_ID_BOOK")
(gw:wrap-value ws 'gnc:id-budget '<gnc:id-type> "GNC_ID_BUDGET")
(gw:wrap-value ws 'gnc:id-lot '<gnc:id-type> "GNC_ID_LOT")
(gw:wrap-value ws 'gnc:id-price '<gnc:id-type> "GNC_ID_PRICE")
(gw:wrap-value ws 'gnc:id-split '<gnc:id-type> "GNC_ID_SPLIT")
(gw:wrap-value ws 'gnc:id-schedxaction '<gnc:id-type> "GNC_ID_SCHEDXACTION")
(gw:wrap-value ws 'gnc:id-session '<gnc:id-type> "GNC_ID_SESSION")
(gw:wrap-value ws 'gnc:id-trans '<gnc:id-type> "GNC_ID_TRANS")

;
; Definitions for query parameter names
;
(gw:wrap-value ws 'gnc:query-default-sort '<gnc:id-type> "QUERY_DEFAULT_SORT")
(gw:wrap-value ws 'gnc:split-lot '<gnc:id-type> "SPLIT_LOT")
(gw:wrap-value ws 'gnc:split-trans '<gnc:id-type> "SPLIT_TRANS")
(gw:wrap-value ws 'gnc:split-account '<gnc:id-type> "SPLIT_ACCOUNT")
(gw:wrap-value ws 'gnc:split-value '<gnc:id-type> "SPLIT_VALUE")
(gw:wrap-value ws 'gnc:split-memo '<gnc:id-type> "SPLIT_MEMO")
(gw:wrap-value ws 'gnc:split-date-reconciled '<gnc:id-type> "SPLIT_DATE_RECONCILED")

(gw:wrap-value ws 'gnc:trans-date-posted '<gnc:id-type> "TRANS_DATE_POSTED")
(gw:wrap-value ws 'gnc:trans-desc '<gnc:id-type> "TRANS_DESCRIPTION")
(gw:wrap-value ws 'gnc:trans-num '<gnc:id-type> "TRANS_NUM")

(gw:wrap-value ws 'gnc:account-name '<gnc:id-type> "ACCOUNT_NAME_")
(gw:wrap-value ws 'gnc:account-code '<gnc:id-type> "ACCOUNT_CODE_")

;
; Special query parameter "sort" types
;
(gw:wrap-value ws 'gnc:split-account-fullname '<gnc:id-type>
	       "SPLIT_ACCT_FULLNAME")
(gw:wrap-value ws 'gnc:split-corr-account-fullname '<gnc:id-type>
	       "SPLIT_CORR_ACCT_NAME")
(gw:wrap-value ws 'gnc:split-corr-account-code '<gnc:id-type>
	       "SPLIT_CORR_ACCT_CODE")

;
; Transaction Types
;
(gw:wrap-value ws 'gnc:transaction-type-none '<gw:char> "TXN_TYPE_NONE")
(gw:wrap-value ws 'gnc:transaction-type-invoice '<gw:char> "TXN_TYPE_INVOICE")
(gw:wrap-value ws 'gnc:transaction-type-payment '<gw:char> "TXN_TYPE_PAYMENT")


(gw:wrap-function
 ws
 'gnc:guid-new
 '<gnc:guid-scm>
 "guid_new_return"
 '()
 "Return a newly-generated GUID.")

(gw:wrap-function
 ws
 'gnc:split-get-guid
 '<gnc:guid-scm>
 "xaccSplitReturnGUID"
 '((<gnc:Split*> s))
 "Return the GUID of Split s.")

(gw:wrap-function
 ws
 'gnc:split-lookup
 '<gnc:Split*>
 "xaccSplitLookupDirect"
 '((<gnc:guid-scm> guid)
   (<gnc:Book*> book))
 "Lookup a split with its GUID.")

(gw:wrap-function
 ws
 'gnc:split-get-balance
 '<gnc:numeric>
 "xaccSplitGetBalance"
 '((<gnc:Split*> s))
 "Return balance at split.")

(gw:wrap-function
 ws
 'gnc:split-get-memo
 '(<gw:mchars> callee-owned const)
 "xaccSplitGetMemo"
 '((<gnc:Split*> s))
 "Return split's memo.")

(gw:wrap-function
 ws
 'gnc:split-get-action
 '(<gw:mchars> callee-owned const)
 "xaccSplitGetAction"
 '((<gnc:Split*> s))
 "Return split's action.")

(gw:wrap-function
 ws
 'gnc:split-get-reconcile-state
 '<gw:char>
 "xaccSplitGetReconcile"
 '((<gnc:Split*> s))
 "Return split's reconcile state.")

(gw:wrap-function
 ws
 'gnc:split-get-reconciled-date
 '<gnc:time-pair>
 "gnc_split_get_date_reconciled"
 '((<gnc:Split*> s))
 "Return split's reconciliation date.")

(gw:wrap-function
 ws
 'gnc:split-get-amount
 '<gnc:numeric>
 "xaccSplitGetAmount"
 '((<gnc:Split*> s))
 "Return split's amount.")

(gw:wrap-function
 ws
 'gnc:split-get-share-price
 '<gnc:numeric>
 "xaccSplitGetSharePrice"
 '((<gnc:Split*> s))
 "Return split's share price.")

(gw:wrap-function
 ws
 'gnc:split-get-value
 '<gnc:numeric>
 "xaccSplitGetValue"
 '((<gnc:Split*> s))
 "Return split's value.")


(gw:wrap-function
 ws
 'gnc:account-separator-string
 '(<gw:mchars> callee-owned const)
 "gnc_get_account_separator_string"
 '()
 "Returns a string with the user-selected account separator")

(gw:wrap-function
 ws
 'gnc:split-get-account
 '<gnc:Account*>
 "xaccSplitGetAccount"
 '((<gnc:Split*> s))
 "Return split's account.")

(gw:wrap-function
 ws
 'gnc:split-get-parent
 '<gnc:Transaction*>
 "xaccSplitGetParent"
 '((<gnc:Split*> s))
 "Return the parent transaction of the split.")

(gw:wrap-function
 ws
 'gnc:split-get-other-split
 '<gnc:Split*>
 "xaccSplitGetOtherSplit"
 '((<gnc:Split*> s))
 "Return the 'other' split of the parent transaction or NULL.")

(gw:wrap-function
 ws
 'gnc:split-compare-account-full-names
 '<gw:int>
 "xaccSplitCompareAccountFullNames"
 '((<gnc:Split*> sa) (<gnc:Split*> sb))
 "Compare two splits on the full names of their parent accounts")

(gw:wrap-function
 ws
 'gnc:split-compare-account-codes
 '<gw:int>
 "xaccSplitCompareAccountCodes"
 '((<gnc:Split*> sa) (<gnc:Split*> sb))
 "Compare two splits on the codes of their parent accounts")

(gw:wrap-function
 ws
 'gnc:split-compare-other-account-full-names
 '<gw:int>
 "xaccSplitCompareOtherAccountFullNames"
 '((<gnc:Split*> sa) (<gnc:Split*> sb))
 "Compare two splits on the full names of the *other* 
split in the transaction")

(gw:wrap-function
 ws
 'gnc:split-compare-other-account-codes
 '<gw:int>
 "xaccSplitCompareOtherAccountCodes"
 '((<gnc:Split*> sa) (<gnc:Split*> sb))
 "Compare two splits on the codes of the *other* split in the transaction")

(gw:wrap-function
 ws
 'gnc:split-get-corr-account-name
 '(<gw:mchars> callee-owned const)
 "xaccSplitGetCorrAccountName"
 '((<gnc:Split*> sa))
 "Find the split on the other side of the transaction, and return the name of
its account")

(gw:wrap-function
 ws
 'gnc:split-get-corr-account-full-name-internal
 '(<gw:gchars> caller-owned)
 "xaccSplitGetCorrAccountFullName"
 '((<gnc:Split*> sa))
 "Find the split on the other side of the transaction, and return the 
name of its account.  Don't use directly, use 
gnc:split-get-corr-account-full-name in src/scm/report-utilities.scm")
(gw:wrap-function
 ws
 'gnc:split-get-corr-account-code
 '(<gw:mchars> callee-owned const)
 "xaccSplitGetCorrAccountCode" 
 '((<gnc:Split*> sa))
 "Find the split on the other side of the transaction, and return the 
code of its account")

(gw:wrap-function
 ws
 'gnc:split-get-lot
 '<gnc:Lot*>
 "xaccSplitGetLot"
 '((<gnc:Split*> s))
 "Return the LOT of Split s.")

(gw:wrap-function
 ws
 'gnc:transaction-get-guid
 '<gnc:guid-scm>
 "xaccTransReturnGUID"
 '((<gnc:Transaction*> t))
 "Return the GUID of Transaction t.")

(gw:wrap-function
 ws
 'gnc:transaction-lookup
 '<gnc:Transaction*>
 "xaccTransLookupDirect"
 '((<gnc:guid-scm> guid)
   (<gnc:Book*> book))
 "Lookup a transaction with its GUID.")

(gw:wrap-function
 ws
 'gnc:transaction-get-split
 '<gnc:Split*>
 "xaccTransGetSplit"
 '((<gnc:Transaction*> t) (<gw:int> i))
 "Returns a pointer to each of the splits in this transaction.  Valid
values for i are zero to (number_of__splits-1).  An invalid value of i
will cause NULL to be returned.  A convenient way of cycling through
all splits is to start at zero, and kep incrementing until a null
pointer is returned.")

(gw:wrap-function
 ws
 'gnc:transaction-get-splits
 '(gw:glist-of <gnc:Split*> callee-owned)
 "xaccTransGetSplitList"
 '((<gnc:Transaction*> t))
 "Returns a list of the splits in t.")

(gw:wrap-function
 ws
 'gnc:transaction-get-num
 '(<gw:mchars> callee-owned const)
 "xaccTransGetNum"
 '((<gnc:Transaction*> t))
 "Return the transaction's num, an arbitrary user-assigned field.  It
is intended to store a short id number, typically the check number,
deposit number, invoice number or other tracking number.")

(gw:wrap-function
 ws
 'gnc:transaction-get-description
 '(<gw:mchars> callee-owned const)
 "xaccTransGetDescription"
 '((<gnc:Transaction*> t))
 "Return the transaction description, an arbitrary user-assigned
value.  It is meant to be a short descriptive phrase.")

(gw:wrap-function
 ws
 'gnc:transaction-get-notes
 '(<gw:mchars> callee-owned const)
 "xaccTransGetNotes"
 '((<gnc:Transaction*> t))
 "Return the transaction notes field.")

(gw:wrap-function
 ws
 'gnc:transaction-void
 '<gw:void>
 "xaccTransVoid"
 '((<gnc:Transaction*> transaction)
   ((<gw:mchars> caller-owned const) reason))
 "Void a transaction")

(gw:wrap-function
 ws
 'gnc:transaction-get-void-status
 '<gw:bool>
 "xaccTransGetVoidStatus"
 '((<gnc:Transaction*> transaction))
 "Return true if a transaction has been voided")

(gw:wrap-function
 ws
 'gnc:transaction-get-void-reason
 '(<gw:mchars> callee-owned const)
 "xaccTransGetVoidReason"
 '((<gnc:Transaction*> transaction))
 "return a string indicating reason for voiding")

(gw:wrap-function
 ws
 'gnc:transaction-order
 '<gw:int>
 "xaccTransOrder"
 '((<gnc:Transaction*> ta) (<gnc:Transaction*> tb))
 "Return an integer for ta,tb ordering.  <0 is ta is before tb, >0 if ta is after tb")

(gw:wrap-function
 ws
 'gnc:split-void-former-amount
 '<gnc:numeric>
 "xaccSplitVoidFormerAmount"
 '((<gnc:Split*> split))
 "get what the split's amount before voiding")

(gw:wrap-function
 ws
 'gnc:split-void-former-value
 '<gnc:numeric>
 "xaccSplitVoidFormerValue"
 '((<gnc:Split*> split))
 "get what the split's value was before voiding")

(gw:wrap-function
 ws
 'gnc:dmy2timespec
 '<gnc:time-pair>
 "gnc_dmy2timespec"
 '((<gw:int> day) (<gw:int> month) (<gw:int> year))
 "Return a given day, month, and year as a pair where the car is the
number of seconds and the cdr is the number of nanoseconds.")

(gw:wrap-function
 ws
 'gnc:transaction-get-date-posted
 '<gnc:time-pair>
 "gnc_transaction_get_date_posted"
 '((<gnc:Transaction*> t))
 "Return the date the transaction was posted at the bank as a pair of
integers.  The car is the number of seconds and the cdr is the number
of nanoseconds.")

(gw:wrap-function
 ws
 'gnc:transaction-get-date-entered
 '<gnc:time-pair>
 "gnc_transaction_get_date_entered"
 '((<gnc:Transaction*> t))
 "Return the date the transaction was entered into the register as a
pair of integers.  The car is the number of seconds and the cdr is the
number of nanoseconds.")

(gw:wrap-function
 ws
 'gnc:transaction-set-date-posted
 '<gw:void>
 "gnc_transaction_set_date_posted"
 '((<gnc:Transaction*> t) (<gnc:time-pair> date))
 "Modifies the posted date of the transaction.  Footnote: this
shouldn't matter to a user, but anyone modifying the engine should
understand that when the transaction is committed, the date order of
each of the component splits will be checked, and they will be
restored in ascending date order.  The date given should be a pair of
integers.  The car is the number of seconds and the cdr is the number
of nanoseconds.")

(gw:wrap-function
 ws
 'gnc:transaction-set-date-entered
 '<gw:void>
 "gnc_transaction_set_date_entered"
 '((<gnc:Transaction*> t) (<gnc:time-pair> date))
 "Modifies entered date of the transaction. The date given should be a
pair of integers.  The car is the number of seconds and the cdr is the
number of nanoseconds.")

(gw:wrap-function
 ws
 'gnc:transaction-get-split-count
 '<gw:int>
 "xaccTransCountSplits"
 '((<gnc:Transaction*> t))
 "Return the number of splits in the transaction.")

(gw:wrap-function
 ws
 'gnc:transaction-get-currency
 '<gnc:commodity*>
 "xaccTransGetCurrency"
 '((<gnc:Transaction*> trans))
 "Returns the commodity common for this transaction. ATM it gives the same result as xaccTransFindCommonCurrency.")

(gw:wrap-function
 ws
 'gnc:transaction-set-currency
 '<gw:void>
 "xaccTransSetCurrency"
 '((<gnc:Transaction*> trans) (<gnc:commodity*> comm))
 "Sets the commodity common for this transaction.")

(gw:wrap-function
 ws
 'gnc:transaction-get-account-value
 '<gnc:numeric>
 "xaccTransGetAccountValue"
 '((<gnc:Transaction*> trans) (<gnc:Account*> acc))
 "Compute the sum of all Splits in trans that are applied to Account acc.")

(gw:wrap-function
 ws
 'gnc:transaction-get-txn-type
 '<gw:char>
 "xaccTransGetTxnType"
 '((<gnc:Transaction*> trans))
 "Return the transaction type.")

(gw:wrap-function
 ws
 'gnc:malloc-account
 '<gnc:Account*>
 "xaccMallocAccount"
 '((<gnc:Book*> book))
 "Allocate a new account structure.")

(gw:wrap-function
 ws
 'gnc:account-destroy
 '<gw:void>
 "xaccAccountDestroy"
 '((<gnc:Account*> account))
 "Free an account structure. (Must call gnc:account-begin-edit first)")

(let ((docstr
       "The gnc:account-begin-edit and gnc:account-commit-edit
 subroutines provide a two-phase-commit wrapper for account updates."))
  (gw:wrap-function
   ws
   'gnc:account-begin-edit
   '<gw:void>
   "xaccAccountBeginEdit"
   '((<gnc:Account*> a))
   docstr)
  (gw:wrap-function
   ws
   'gnc:account-commit-edit
   '<gw:void>
   "xaccAccountCommitEdit"
   '((<gnc:Account*> a))
   docstr))

(gw:wrap-function
 ws
 'gnc:account-insert-split
 '<gw:void>
 "xaccAccountInsertSplit"
 '((<gnc:Account*> a) (<gnc:Split*> s))
 "Insert the split s into account a. If the split already belongs
to another account, it will be removed from that account first.")

;; (gw:wrap-function
;;  ws
;;  'gnc:account-order
;;  '<gw:int>
;;  "xaccAccountOrder_gwrap"
;;  '(
;;    (<gnc:Account**> a1)
;;    (<gnc:Account**> a2)
;;    )
;;  "Defines a sorting order on accounts.  Returns -1 if a1 is \"less
;; than\" the second, +1 if the a1 is \"greater than\" the second, and 0
;; if they are equal.  To determine the sort order, the account codes are
;; compared, and if these are equal, then account types, and, if these
;; are equal, then account names.")

(gw:wrap-function
 ws
 'gnc:account-set-type
 '<gw:void>
 "xaccAccountSetType"
 '((<gnc:Account*> a) (<gnc:AccountType> type))
 "Set Account type.  See definition of scheme GNCAccountType for values.")

(gw:wrap-function
 ws
 'gnc:account-set-name
 '<gw:void>
 "xaccAccountSetName"
 '((<gnc:Account*> a) ((<gw:mchars> caller-owned const) name))
 "Set account name")

(gw:wrap-function
 ws
 'gnc:account-set-code
 '<gw:void>
 "xaccAccountSetCode"
 '((<gnc:Account*> a) ((<gw:mchars> caller-owned const) code))
 "Set whatever cryptic code we wish to use as the alternative to\n"
 "the account name.\n"
 "\n"
 "Commonly these codes are numeric, and organize asset accounts together\n"
 "in one number range, liability accounts in another number range, and\n"
 "so forth...\n")

(gw:wrap-function
 ws
 'gnc:account-set-description
 '<gw:void>
 "xaccAccountSetDescription"
 '((<gnc:Account*> a) ((<gw:mchars> caller-owned const) description))
 "Set a slightly-more-verbose description for the account.")

(gw:wrap-function
 ws
 'gnc:account-set-notes
 '<gw:void>
 "xaccAccountSetNotes"
 '((<gnc:Account*> a) ((<gw:mchars> caller-owned const) notes))
 "Set up a longer set of notes that provide a perhaps-very-verbose
description of the nature of a particular account.")

(gw:wrap-function
 ws
 'gnc:account-set-tax-related
 '<gw:void>
 "xaccAccountSetTaxRelated"
 '((<gnc:Account*> a) (<gw:bool> tax-related))
 "Set the tax-related flag of the account.")

(gw:wrap-function
 ws
 'gnc:account-get-guid
 '<gnc:guid-scm>
 "xaccAccountReturnGUID"
 '((<gnc:Account*> a))
 "Get the GUID of Account a.")

(gw:wrap-function
 ws
 'gnc:account-lookup
 '<gnc:Account*>
 "xaccAccountLookupDirect"
 '((<gnc:guid-scm> guid) (<gnc:Book*> book))
 "Lookup the account with GUID guid.")

(gw:wrap-function
 ws
 'gnc:account-get-type-string
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetTypeStr"
 '((<gnc:AccountType> type))
 "What's the account type's name.")

(gw:wrap-function
 ws
 'gnc:account-get-type
 '<gnc:AccountType>
 "xaccAccountGetType"
 '((<gnc:Account*> a))
 "What's the account type?  See scheme GNCAccountType enum def values.")

(gw:wrap-function
 ws
 'gnc:account-get-name
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetName"
 '((<gnc:Account*> a))
 "Get the brief name for the account.")

(gw:wrap-function
 ws
 'gnc:account-get-code
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetCode"
 '((<gnc:Account*> a))
 "Get the account's ``account code.''  

Commonly this is used to provide a hierarchy where accounts with
similar classifications (e.g. - Assets, Liabilities, Equity, Income,
Expenses) are given numeric codes in corresponding ``number ranges.''")

(gw:wrap-function
 ws
 'gnc:account-get-description
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetDescription"
 '((<gnc:Account*> a))
 "Get the slightly-verbose description of the account.")

(gw:wrap-function
 ws
 'gnc:account-get-notes
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetNotes"
 '((<gnc:Account*> a))
 "Get the fully-verbose description of the account.")

(gw:wrap-function
 ws
 'gnc:account-get-commodity
 '(<gnc:commodity*> const)
 "xaccAccountGetCommodity"
 '((<gnc:Account*> a))
 "Get the commodity in which the account is denominated.")

(gw:wrap-function
 ws
 'gnc:account-set-commodity
 '<gw:void>
 "xaccAccountSetCommodity"
 '((<gnc:Account*> a) (<gnc:commodity*> comm))
 "Set the commodity in which the account is denominated.")

(gw:wrap-function
 ws
 'gnc:account-get-tax-related
 '<gw:bool>
 "xaccAccountGetTaxRelated"
 '((<gnc:Account*> a))
 "Get the tax related flag of the account.")

(gw:wrap-function
 ws
 'gnc:account-get-tax-US-code
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetTaxUSCode"
 '((<gnc:Account*> a))
 "Get the tax code set on the account.")

(gw:wrap-function
 ws
 'gnc:account-get-tax-US-payer-name-source
 '(<gw:mchars> callee-owned const)
 "xaccAccountGetTaxUSPayerNameSource"
 '((<gnc:Account*> a))
 "Get the tax payer name source set on the account.")

(gw:wrap-function
 ws
 'gnc:account-get-children
 '<gnc:AccountGroup*>
 "xaccAccountGetChildren"
 '((<gnc:Account*> a))
 "Get a pointer to an AccountGroup that represents the set of
children to this account.")

(gw:wrap-function
 ws
 'gnc:account-get-parent
 '<gnc:AccountGroup*>
 "xaccAccountGetParent"
 '((<gnc:Account*> a))
 "Get the pointer to the account's parent.")

(gw:wrap-function
 ws
 'gnc:account-get-parent-account
 '<gnc:Account*>
 "xaccAccountGetParentAccount"
 '((<gnc:Account*> a))
 "Get the pointer to the account's parent account.")

(gw:wrap-function
 ws
 'gnc:account-get-balance
 '<gnc:numeric>
 "xaccAccountGetBalance"
 '((<gnc:Account*> a))
 "Undocumented.")

(gw:wrap-function
 ws
 'gnc:account-get-cleared-balance
 '<gnc:numeric>
 "xaccAccountGetClearedBalance"
 '((<gnc:Account*> a))
 "Undocumented.")

(gw:wrap-function
 ws
 'gnc:account-get-reconciled-balance
 '<gnc:numeric>
 "xaccAccountGetReconciledBalance"
 '((<gnc:Account*> a))
 "Undocumented.")

(gw:wrap-function
 ws
 'gnc:account-get-split-list
 '(gw:glist-of <gnc:Split*> callee-owned)
 "xaccAccountGetSplitList"
 '((<gnc:Account*> a))
 "Get a list of the of splits in account a.")

(gw:wrap-function
 ws
 'gnc:malloc-account-group
 '<gnc:AccountGroup*>
 "xaccMallocAccountGroup"
 '((<gnc:Book*> book))
 "Create a new account group.")

(gw:wrap-function
 ws
 'gnc:account-group-begin-edit
 '<gw:void>
 "xaccAccountGroupBeginEdit"
 '((<gnc:AccountGroup*> g))
 "Open an account group for editing.")

(gw:wrap-function
 ws
 'gnc:account-group-commit-edit
 '<gw:void>
 "xaccAccountGroupCommitEdit"
 '((<gnc:AccountGroup*> g))
 "Commit all changes to an account group.")

(gw:wrap-function
 ws
 'gnc:account-group-destroy
 '<gw:void>
 "xaccAccountGroupDestroy"
 '((<gnc:AccountGroup*> g))
 "Free an account group. (Must call gnc:account-group-begin-edit first)")

(gw:wrap-function
 ws
 'gnc:group-get-book
 '<gnc:Book*>
 "xaccGroupGetBook"
 '((<gnc:AccountGroup*> g))
 "Return the QofBook of group g.")

(gw:wrap-function
 ws
 'gnc:group-merge-accounts
 '<gw:void>
 "xaccGroupMergeAccounts"
 '((<gnc:AccountGroup*> g))
 "Merge accounts which have the same name and description. Used in
importing Quicken files.")

(gw:wrap-function
 ws
 'gnc:group-concat-group
 '<gw:void>
 "xaccGroupConcatGroup"
 '((<gnc:AccountGroup*> old)
   (<gnc:AccountGroup*> new))
 "Catenate accounts from one group into another. Used in Quicken
import.")

(gw:wrap-function
 ws
 'gnc:group-get-num-subaccounts
 '<gw:int>
 "xaccGroupGetNumSubAccounts"
 '((<gnc:AccountGroup*> g))
 "Return the number of accounts, including subaccounts, in the account
group")

(gw:wrap-function
 ws
 'gnc:group-get-num-accounts
 '<gw:int>
 "xaccGroupGetNumAccounts"
 '((<gnc:AccountGroup*> g))
 "Return the number of accounts in the indicated group only"
 "(children not counted).")

(gw:wrap-function
 ws
 'gnc:group-get-account
 '<gnc:Account*>
 "xaccGroupGetAccount"
 '((<gnc:AccountGroup*> g) (<gw:int> n))
 "Return account number n in account group g.")

(gw:wrap-function
 ws
 'gnc:get-account-from-full-name
 '<gnc:Account*>
 "xaccGetAccountFromFullName"
 '((<gnc:AccountGroup*> g)
   ((<gw:mchars> caller-owned const) name))
 "Return account named name in group g.  full path with separators.")

(gw:wrap-function
 ws
 'gnc:group-get-parent
 '<gnc:Account*>
 "xaccGroupGetParentAccount"
 '((<gnc:AccountGroup*> g))
 "Return the parent acount for the group.")

(gw:wrap-function
 ws
 'gnc:group-insert-account
 '<gw:void>
 "xaccGroupInsertAccount"
 '((<gnc:AccountGroup*> g) (<gnc:Account*> a))
 "Add account a to group g.")

(gw:wrap-function
 ws
 'gnc:account-insert-subaccount
 '<gw:void>
 "xaccAccountInsertSubAccount"
 '((<gnc:Account*> p) (<gnc:Account*> c))
 "Add a child account c to parent p")

(gw:wrap-function
 ws
 'gnc:group-get-subaccounts
 '(gw:glist-of <gnc:Account*> caller-owned)
 "xaccGroupGetSubAccountsSorted"
 '((<gnc:AccountGroup*> g))
 "Return a list containing all of the accounts, including
subaccounts, in the account group. The returned array should be freed
when no longer needed.")

(gw:wrap-function
 ws
 'gnc:group-get-account-list
 '(gw:glist-of <gnc:Account*> caller-owned)
 "xaccGroupGetAccountListSorted"
 '((<gnc:AccountGroup*> g))
 "Return a list containing the immediate children of g.")

(gw:wrap-function
 ws
 'gnc:group-begin-staged-transaction-traversals
 '<gw:void>
 "xaccGroupBeginStagedTransactionTraversals"
 '((<gnc:AccountGroup*> group))
 "Sets things up to begin a sequence of staged traversals.")

(gw:wrap-function
 ws
 'gnc:group-staged-transaction-traversal
 '<gw:bool>
 "gnc_scmGroupStagedTransactionTraversal"
 '((<gnc:AccountGroup*> group)
   (<gw:unsigned-int> stage)
   (<gw:scm> thunk))
 "FIXME: For now, see Group.h for info...")

(gw:wrap-function
 ws
 'gnc:account-staged-transaction-traversal
 '<gw:bool>
 "gnc_scmAccountStagedTransactionTraversal"
 '((<gnc:Account*> account)
   (<gw:unsigned-int> stage)
   (<gw:scm> thunk))
 "FIXME: For now, see Group.h for info...")

(gw:wrap-function
 ws
 'gnc:account-get-lot-list
 '(gw:glist-of <gnc:Lot*> callee-owned)
 "xaccAccountGetLotList"
 '((<gnc:Account*> account))
 "Return the list of Lots for this account.")

;;============
;; GNCPriceDB

(gw:wrap-as-wct ws '<gnc:PriceDB*> "GNCPriceDB *" "const GNCPriceDB *")
(gw:wrap-as-wct ws '<gnc:Price*> "GNCPrice *" "const GNCPrice *")

(gw:wrap-function
 ws
 'gnc:price-create
 '<gnc:Price*>
 "gnc_price_create"
 '((<gnc:Book*> book))
 "Create and return a new price.")

(gw:wrap-function
 ws
 'gnc:price-get-guid
 '<gnc:guid-scm>
 "gnc_price_return_guid"
 '((<gnc:Price*> a))
 "Get the GUID of a price.")

(gw:wrap-function
 ws
 'gnc:price-unref
 '<gw:void>
 "gnc_price_unref"
 '((<gnc:Price*> p))
 "Indicate you're finished with this price.")

(gw:wrap-function
 ws
 'gnc:price-set-commodity
 '<gw:void>
 "gnc_price_set_commodity"
 '((<gnc:Price*> p) (<gnc:commodity*> c))
 "Set the price's commodity.")

(gw:wrap-function
 ws
 'gnc:price-set-currency
 '<gw:void>
 "gnc_price_set_currency"
 '((<gnc:Price*> p) (<gnc:commodity*> c))
 "Set the price's currency.")

(gw:wrap-function
 ws
 'gnc:price-set-time
 '<gw:void>
 "gnc_price_set_time"
 '((<gnc:Price*> p) (<gnc:time-pair> t))
 "Set the price's time stamp.")

(gw:wrap-function
 ws
 'gnc:price-set-source
 '<gw:void>
 "gnc_price_set_source"
 '((<gnc:Price*> p) ((<gw:mchars> caller-owned) src))
 "Set the price's source.")

(gw:wrap-function
 ws
 'gnc:price-set-type
 '<gw:void>
 "gnc_price_set_type"
 '((<gnc:Price*> p) ((<gw:mchars> caller-owned) type))
 "Set the price's type.")

(gw:wrap-function
 ws
 'gnc:price-set-value
 '<gw:void>
 "gnc_price_set_value"
 '((<gnc:Price*> p) (<gnc:numeric> value))
 "Set the price's value.")

(gw:wrap-function
 ws
 'gnc:price-get-value
 '<gnc:numeric>
 "gnc_price_get_value"
 '((<gnc:Price*> p))
 "Get the price's value")

(gw:wrap-function
 ws
 'gnc:price-get-commodity
 '<gnc:commodity*> 
 "gnc_price_get_commodity"
 '((<gnc:Price*> p))
 "Get the commodity this price is for.")

(gw:wrap-function
 ws
 'gnc:price-get-currency
 '<gnc:commodity*> 
 "gnc_price_get_currency"
 '((<gnc:Price*> p))
 "Get the currency (commodity) this price's value is denominated in.")

(gw:wrap-function
 ws
 'gnc:price-get-time
 '<gnc:time-pair>
 "gnc_price_get_time"
 '((<gnc:Price*> p))
 "Get the time stamp of this price.")

(gw:wrap-function
 ws
 'gnc:pricedb-add-price
 '<gw:bool>
 "gnc_pricedb_add_price"
 '((<gnc:PriceDB*> db) (<gnc:Price*> p))
 "Add a price to the DB.  Unref the price when you're finished with it.")

(gw:wrap-function
 ws
 'gnc:price-list-destroy
 '<gw:void>
 "gnc_price_list_destroy"
 '(((gw:glist-of <gnc:Price*> callee-owned) prices))
 "Destroys a gnc price list unrefing the prices included in the list")

(gw:wrap-function
 ws
 'gnc:pricedb-lookup-latest
 '<gnc:Price*>
 "gnc_pricedb_lookup_latest"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity) (<gnc:commodity*> currency))
 "Returns the latest price.  Unref the price when you're finished with it.")

(gw:wrap-function
 ws
 'gnc:pricedb-lookup-latest-any-currency
 '(gw:glist-of <gnc:Price*> caller-owned)
 "gnc_pricedb_lookup_latest_any_currency"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity))
 "Returns the latest price(s) in any currency available.")

(gw:wrap-function
 ws
 'gnc:pricedb-lookup-nearest-in-time
 '<gnc:Price*>
 "gnc_pricedb_lookup_nearest_in_time"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity) (<gnc:commodity*> currency)
   (<gnc:time-pair> t))
 "Returns the price quote nearest to t.  Unref price when finished with it.")

(gw:wrap-function
 ws
 'gnc:pricedb-lookup-nearest-in-time-any-currency
 '(gw:glist-of <gnc:Price*> caller-owned)
 "gnc_pricedb_lookup_nearest_in_time_any_currency"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity) (<gnc:time-pair> t))
 "Returns the price(s) nearest to t in any currency available.")


(gw:wrap-function
ws
'gnc:pricedb-lookup-latest-before
'<gnc:Price*>
"gnc_pricedb_lookup_latest_before"
'((<gnc:PriceDB*> db)
  (<gnc:commodity*> commodity) (<gnc:commodity*> currency)
  (<gnc:time-pair> t))
"Returns the latest price quote <= t. Unref price when finished with it.")

(gw:wrap-function
ws
'gnc:pricedb-lookup-latest-before-any-currency
'(gw:glist-of <gnc:Price*> caller-owned)
"gnc_pricedb_lookup_latest_before_any_currency"
'((<gnc:PriceDB*> db)
  (<gnc:commodity*> commodity) (<gnc:time-pair> t))
"Returns the latest price quote(s) <= t in any currency available.")


(gw:wrap-function
 ws
 'gnc:pricedb-get-prices
 '(gw:glist-of <gnc:Price*> caller-owned)
 "gnc_pricedb_get_prices"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity)
   (<gnc:commodity*> currency))
 "Get all prices for commodity in currency.")

(gw:wrap-function
 ws
 'gnc:pricedb-lookup-at-time
 '(gw:glist-of <gnc:Price*> caller-owned)
 "gnc_pricedb_lookup_at_time"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity) (<gnc:commodity*> currency)
   (<gnc:time-pair> t))
 "Lookup a price at time t.")

(gw:wrap-function
 ws
 'gnc:pricedb-lookup-day
 '(gw:glist-of <gnc:Price*> caller-owned)
 "gnc_pricedb_lookup_day"
 '((<gnc:PriceDB*> db)
   (<gnc:commodity*> commodity) (<gnc:commodity*> currency)
   (<gnc:time-pair> t))
 "Lookup a price on the day specified by time t.")

(gw:wrap-function
 ws
 'gnc:pricedb-convert-balance-latest-price
 '<gnc:numeric>
 "gnc_pricedb_convert_balance_latest_price"
 '((<gnc:PriceDB*> db)
   (<gnc:numeric> balance)
   (<gnc:commodity*> balance_commodity) (<gnc:commodity*> new_currency))
 "convert balance in commodity balance_commodity to new_currency using latest price.")

(gw:wrap-function
 ws
 'gnc:pricedb-convert-balance-nearest-price
 '<gnc:numeric>
 "gnc_pricedb_convert_balance_nearest_price"
 '((<gnc:PriceDB*> db)
   (<gnc:numeric> balance)
   (<gnc:commodity*> balance_commodity) (<gnc:commodity*> new_currency)
   (<gnc:time-pair> t))
 "convert balance in commodity balance_commodity to new_currency using nearest price
to time t.")


(gw:wrap-function
 ws
 'gnc:pricedb-convert-balance-latest-before
 '<gnc:numeric>
 "gnc_pricedb_convert_balance_latest_before"
 '((<gnc:PriceDB*> db)
   (<gnc:numeric> balance)
   (<gnc:commodity*> balance_commodity) (<gnc:commodity*> new_currency)
   (<gnc:time-pair> t))
 "convert balance in commodity balance_commodity to new_currency using latest price
prior to time t.")


;;===========
;; QofSession

(gw:wrap-function
 ws
 'gnc:session-new
 '<gnc:Session*>
 "qof_session_new" '()
 "Create a new session.")

(gw:wrap-function
 ws
 'gnc:session-destroy
 '<gw:void>
 "qof_session_destroy"
 '((<gnc:Session*> session))
 "Destroy the given session.")

(gw:wrap-function
 ws
 'gnc:session-get-book
 '<gnc:Book*>
 "qof_session_get_book"
 '((<gnc:Session*> session))
 "Get the book of the given session.")

(gw:wrap-function
 ws
 'gnc:session-begin
 '<gw:void>
 "qof_session_begin"
 '((<gnc:Session*> session)
   ((<gw:mchars> caller-owned const) id)
   (<gw:bool> ignore-lock?)
   (<gw:bool> create-if-nonexistent?))
 "Setup the session for use.")

(gw:wrap-function
 ws
 'gnc:session-load
 '<gw:void>
 "gnc_session_scm_load"
 '((<gnc:Session*> session))
 "Load the data associated with the given session.")

(gw:wrap-function
 ws
 'gnc:session-save
 '<gw:void>
 "gnc_session_scm_save"
 '((<gnc:Session*> session))
 "Save the data in the session.")

(gw:wrap-function
 ws
 'gnc:session-set-callback
 '<gw:void>
 "gnc_session_scm_set_callback"
 '((<gw:scm> callback))
 "Setup a callback for the load/save functions to provide progress
reports. This function will be called with a string and an integer
argument between 0 and 100 (inclusive).")

(gw:wrap-function
 ws
 'gnc:session-end
 '<gw:void>
 "qof_session_end"
 '((<gnc:Session*> session))
 "Indicate you're finished with the session.")

(gw:wrap-function
 ws
 'gnc:book-get-group
 '<gnc:AccountGroup*>
 "xaccGetAccountGroup"
 '((<gnc:Book*> book))
 "Get the book's account group.")

(gw:wrap-function
 ws
 'gnc:book-get-template-group
 '<gnc:AccountGroup*>
 "gnc_book_get_template_group"
 '((<gnc:Book*> book))
 "Get the book's template account group.")

(gw:wrap-function
 ws
 'gnc:book-get-commodity-table
 '<gnc:commodity-table*>
 "gnc_commodity_table_get_table"
 '((<gnc:Book*> book))
 "Get the book's commodity table.")

(gw:wrap-function
 ws
 'gnc:book-get-pricedb
 '<gnc:PriceDB*>
 "gnc_pricedb_get_db"
 '((<gnc:Book*> book))
 "Get the book's pricedb.")

(gw:wrap-function
 ws
 'gnc:book-kvp-changed
 '<gw:void>
 "qof_book_kvp_changed"
 '((<gnc:Book*> book))
 "Set the flag that the Book's kvp changed.")

(gw:wrap-function
 ws
 'gnc:session-get-error
 '<gnc:BackendError>
 "qof_session_get_error"
 '((<gnc:Session*> session))
 "Check for a pending error.")

(gw:wrap-function
 ws
 'gnc:session-get-url
 '(<gw:mchars> callee-owned const)
 "qof_session_get_url"
 '((<gnc:Session*> session))
 "Return the URL of the opened session.")

(gw:wrap-function
 ws
 'gnc:session-pop-error
 '<gnc:BackendError>
 "qof_session_pop_error"
 '((<gnc:Session*> session))
 "Remove an error, if any, from the error stack.")

(gw:wrap-function
 ws
 'gnc:set-log-level-global
 '<gw:void>
 "qof_log_set_level_registered"
 '((<gw:int> level))
 "Set the logging level for all modules to level.")

(gw:wrap-function
 ws
 'gnc:print-date
 '(<gw:mchars> callee-owned const)
 "gnc_print_date"
 '((<gnc:time-pair> date))
 "Returns a string with the date formatted according to the
current settings")

(gw:wrap-function
 ws
 'gnc:transaction-destroy
 '<gw:void>
 "xaccTransDestroy"
 '((<gnc:Transaction*> t))
 "Destroys the transaction in question.")

(gw:wrap-function
 ws
 'gnc:transaction-begin-edit
 '<gw:void>
 "xaccTransBeginEdit"
 '((<gnc:Transaction*> t))
 "Start an edit session on a transaction.")

(gw:wrap-function
 ws
 'gnc:transaction-commit-edit
 '<gw:void>
 "xaccTransCommitEdit"
 '((<gnc:Transaction*> t))
 "Commit edits to a transaction.")

(gw:wrap-function
 ws
 'gnc:transaction-is-open
 '<gw:bool>
 "xaccTransIsOpen"
 '((<gnc:Transaction*> t))
 "Returns true if the transaction t is open for editing.")

(gw:wrap-function
 ws
 'gnc:split-destroy
 '<gw:void>
 "xaccSplitDestroy"
 '((<gnc:Split*> s))
 "Destroys the split in question.  Probably only useful inside the context
of having a parent transaction with which one is working...")

(gw:wrap-function
 ws
 'gnc:transaction-append-split
 '<gw:void>
 "xaccTransAppendSplit"
 '((<gnc:Transaction*> t) (<gnc:Split*> s))
 "Adds a split to a transaction.")

(gw:wrap-function
 ws
 'gnc:transaction-set-date
 '<gw:void>
 "xaccTransSetDate"
 '((<gnc:Transaction*> t)
   (<gw:int> day)
   (<gw:int> month)
   (<gw:int> year))
 "Set date on transaction based on day, month, year values")

(gw:wrap-function
 ws
 'gnc:transaction-set-date-time-pair
 '<gw:void>
 "gnc_transaction_set_date"
 '((<gnc:Transaction*> t) (<gnc:time-pair> date))
 "Set date on transaction based on the time-pair")

(gw:wrap-function
 ws
 'gnc:transaction-set-xnum
 '<gw:void>
 "xaccTransSetNum"
 '((<gnc:Transaction*> t) ((<gw:mchars> caller-owned const) xnum))
 "Set the XNUM - e.g. - cheque number or other identifier")

(gw:wrap-function
 ws
 'gnc:transaction-set-description
 '<gw:void>
 "xaccTransSetDescription"
 '((<gnc:Transaction*> t) ((<gw:mchars> caller-owned const) desc))
 "Set the transaction description.")

(gw:wrap-function
 ws
 'gnc:transaction-set-notes
 '<gw:void>
 "xaccTransSetNotes"
 '((<gnc:Transaction*> t) ((<gw:mchars> caller-owned const) notes))
 "Set the transaction notes field.")

(gw:wrap-function
 ws
 'gnc:transaction-create
 '<gnc:Transaction*>
 "xaccMallocTransaction"
 '((<gnc:Book*> book>))
 "Create a Transaction structure")

(gw:wrap-function
 ws
 'gnc:split-create
 '<gnc:Split*>
 "xaccMallocSplit"
 '((<gnc:Book*> book>>))
 "Create a Split structure")

(gw:wrap-function
 ws
 'gnc:split-set-value
 '<gw:void>
 "xaccSplitSetValue"
 '((<gnc:Split*> s) (<gnc:numeric> amount))
 "Set value for split")

(gw:wrap-function
 ws
 'gnc:split-set-base-value
 '<gw:void>
 "xaccSplitSetBaseValue"
 '((<gnc:Split*> s)
   (<gnc:numeric> amount)
   (<gnc:commodity*> currency))
 "Set value for split with currency")

(gw:wrap-function
 ws
 'gnc:split-set-memo
 '<gw:void>
 "xaccSplitSetMemo"
 '((<gnc:Split*> s) ((<gw:mchars> caller-owned const) memo))
 "Set memo for a split")

(gw:wrap-function
 ws
 'gnc:split-set-action
 '<gw:void>
 "xaccSplitSetAction"
 '((<gnc:Split*> s) ((<gw:mchars> caller-owned const) action))
 "Set Action for a split.")

(gw:wrap-function
 ws
 'gnc:split-set-reconcile
 '<gw:void>
 "xaccSplitSetReconcile"
 '((<gnc:Split*> s) (<gw:char> value))
 "Set reconcile state for split entry")

(gw:wrap-function
 ws
 'gnc:split-set-share-price
 '<gw:void>
 "xaccSplitSetSharePrice"
 '((<gnc:Split*> s) (<gnc:numeric> value))
 "Set share price for split entry")

(gw:wrap-function
 ws
 'gnc:split-set-amount
 '<gw:void>
 "xaccSplitSetAmount"
 '((<gnc:Split*> s) (<gnc:numeric> value))
 "Set amount for split entry")

(gw:wrap-function
 ws
 'gnc:split-set-share-price-and-amount
 '<gw:void>
 "xaccSplitSetSharePriceAndAmount"
 '((<gnc:Split*> s)
   (<gnc:numeric> price)
   (<gnc:numeric> amount))
 "Set the share price and amount for split entry")

(gw:wrap-function
 ws
 'gnc:malloc-query
 '<gnc:Query*>
 "xaccMallocQuery"
 '()
 "Create a new (empty) Query structure to search for splits.")

(gw:wrap-function
 ws
 'gnc:query-create
 '<gnc:Query*>
 "qof_query_create"
 '()
 "Create a new (empty) Query structure.")

(gw:wrap-function
 ws
 'gnc:query-create-for
 '<gnc:Query*>
 "qof_query_create_for"
 '((<gnc:id-type> obj-type))
 "Create a new (empty) Query structure to search for the supplied type.")

(gw:wrap-function
 ws
 'gnc:query-search-for
 '<gw:void>
 "qof_query_search_for"
 '((<gnc:Query*> q) (<gnc:id-type> obj-type))
 "Set the object-type to search for.")

(gw:wrap-function
 ws
 'gnc:free-query
 '<gw:void>
 "xaccFreeQuery"
 '((<gnc:Query*> q))
 "Delete the Query and its terms.")

(gw:wrap-function
 ws
 'gnc:query-set-book
 '<gw:void>
 "xaccQuerySetBook"
 '((<gnc:Query*> q) (<gnc:Book*> book))
 "Set the book that a query pertains to.")

(gw:wrap-function
 ws
 'gnc:query-invert
 '<gnc:Query*>
 "xaccQueryInvert"
 '((<gnc:Query*> q))
 "Logically invert a Query (returns a newly-allocated Query object)")

(gw:wrap-function
 ws
 'gnc:query-merge
 '<gnc:Query*>
 "xaccQueryMerge"
 '((<gnc:Query*> q1) (<gnc:Query*> q2) (<gnc:query-op> qop))
 "Merge two queries (returns a newly allocated object")

(gw:wrap-function
 ws
 'gnc:query-clear
 '<gw:void>
 "xaccQueryClear"
 '((<gnc:Query*> q))
 "Clear the terms from a query object")

(gw:wrap-function
 ws
 'gnc:query-purge-terms
 '<gw:void>
 "qof_query_purge_terms"
 '((<gnc:Query*> q) ((gw:gslist-of <gnc:id-type> caller-owned) param-path))
 "Remove query terms of a particular parameter-path.")

(gw:wrap-function
 ws
 'gnc:query-has-terms?
 '<gw:bool>
 "xaccQueryHasTerms"
 '((<gnc:Query*> q))
 "Check if a Query is clear or has terms.")

(gw:wrap-function
 ws
 'gnc:query-get-splits
 '(gw:glist-of <gnc:Split*> callee-owned)
 "xaccQueryGetSplits"
 '((<gnc:Query*> q))
 "Return a list of splits matching the Query.")

(gw:wrap-function
 ws
 'gnc:query-get-splits-unique-trans
 '(gw:glist-of <gnc:Split*> caller-owned)
 "xaccQueryGetSplitsUniqueTrans"
 '((<gnc:Query*> q))
 "Return a list of splits matching the Query, but at most one per transaction")

(gw:wrap-function
 ws
 'gnc:query-get-transactions
 '(gw:glist-of <gnc:Transaction*> caller-owned)
 "xaccQueryGetTransactions"
 '((<gnc:Query*> q) (<gnc:query-txn-match-t> rt))
 "Find transactions with splits matching the Query.")

(gw:wrap-function
 ws
 'gnc:query-get-lots
 '(gw:glist-of <gnc:Lot*> caller-owned)
 "xaccQueryGetLots"
 '((<gnc:Query*> q) (<gnc:query-txn-match-t> rt))
 "Find lots with splits matching the Query.")

(gw:wrap-function
 ws
 'gnc:query-add-account-match
 '<gw:void>
 "xaccQueryAddAccountMatch"
 '((<gnc:Query*> q)
   ((gw:glist-of <gnc:Account*> caller-owned) accts)
   (<gnc:guid-match-how> acctmatch)
   (<gnc:query-op> how))
 "Match splits against one or all of a set of accounts.")

(gw:wrap-function
 ws
 'gnc:query-add-single-account-match
 '<gw:void>
 "xaccQueryAddSingleAccountMatch"
 '((<gnc:Query*> q) (<gnc:Account*> acct) (<gnc:query-op> how))
 "Match splits against a single account.")

(gw:wrap-function
 ws
 'gnc:query-add-description-match
 '<gw:void>
 "xaccQueryAddDescriptionMatch"
 '((<gnc:Query*> q)
   ((<gw:mchars> caller-owned const) mstring)
   (<gw:int> case-sens)
   (<gw:int> use-regexp)
   (<gnc:query-op> how))
 "Match splits against the transaction description.")

(gw:wrap-function
 ws
 'gnc:query-add-number-match
 '<gw:void>
 "xaccQueryAddNumberMatch"
 '((<gnc:Query*> q)
   ((<gw:mchars> caller-owned const) mstring)
   (<gw:int> case-sens)
   (<gw:int> use-regexp)
   (<gnc:query-op> how))
 "Match splits against the Number field.")

(gw:wrap-function
 ws
 'gnc:query-add-action-match
 '<gw:void>
 "xaccQueryAddActionMatch"
 '((<gnc:Query*> q)
   ((<gw:mchars> caller-owned const) mstring)
   (<gw:int> case-sens)
   (<gw:int> use-regexp)
   (<gnc:query-op> how))
 "Match splits against the Action field (a string).")

(gw:wrap-function
 ws
 'gnc:query-add-share-price-match
 '<gw:void>
 "xaccQueryAddSharePriceMatch"
 '((<gnc:Query*> q)
   (<gnc:numeric> amount)
   (<gnc:query-compare-how> mhow)
   (<gnc:query-op> how))
 "Match splits against the share price (including inequalities)")

(gw:wrap-function
 ws
 'gnc:query-add-shares-match
 '<gw:void>
 "xaccQueryAddSharesMatch"
 '((<gnc:Query*> q)
   (<gnc:numeric> amount)
   (<gnc:query-compare-how> mhow)
   (<gnc:query-op> how))
 "Match splits against the share price (including inequalities)")

(gw:wrap-function
 ws
 'gnc:query-add-value-match
 '<gw:void>
 "xaccQueryAddValueMatch"
 '((<gnc:Query*> q)
   (<gnc:numeric> amount)
   (<gnc:numeric-match-how> match-sign)
   (<gnc:query-compare-how> mhow)
   (<gnc:query-op> how))
 "Match splits against the value (including inequalities)")

(gw:wrap-function
 ws
 'gnc:query-add-date-match
 '<gw:void>
 "xaccQueryAddDateMatch"
 '((<gnc:Query*> q)
   (<gw:bool> use-start)
   (<gw:int> st-day)
   (<gw:int> st-mon)
   (<gw:int> st-year)
   (<gw:bool> use-end)
   (<gw:int> end-day)
   (<gw:int> end-mon)
   (<gw:int> end-year)
   (<gnc:query-op> how))
 "Match the transaction date.")

(gw:wrap-function
 ws
 'gnc:query-add-date-match-timepair
 '<gw:void>
 "xaccQueryAddDateMatchTS"
 '((<gnc:Query*> q)
   (<gw:bool> use-start)
   (<gnc:time-pair> start)
   (<gw:bool> use-end)
   (<gnc:time-pair> end)
   (<gnc:query-op> how))
 "Match the transaction date.")

(gw:wrap-function
 ws
 'gnc:query-add-memo-match
 '<gw:void>
 "xaccQueryAddMemoMatch"
 '((<gnc:Query*> q)
   ((<gw:mchars> caller-owned const) mstring)
   (<gw:int> case-sens)
   (<gw:int> use-regexp)
   (<gnc:query-op> how))
 "Match splits against the Memo field.")

(gw:wrap-function
 ws
 'gnc:query-add-cleared-match
 '<gw:void>
 "xaccQueryAddClearedMatch"
 '((<gnc:Query*> q) (<gnc:cleared-match-how> cleared-how) (<gnc:query-op> how))
 "match splits against the cleared state.")

(gw:wrap-function
 ws
 'gnc:query-add-guid-match
 '<gw:void>
 "xaccQueryAddGUIDMatchGL"
 '((<gnc:Query*> q) ((gw:glist-of <gnc:id-type> callee-owned) param_path)
   (<gnc:guid-scm> guid) (<gnc:query-op> how))
 "Add a GUID match against the specified param_path")

(gw:wrap-function
 ws
 'gnc:query-set-sort-order
 '<gw:void>
 "qof_query_set_sort_order"
 '((<gnc:Query*> q)
   ((gw:gslist-of <gnc:id-type> callee-owned) primary)
   ((gw:gslist-of <gnc:id-type> callee-owned) secondary)
   ((gw:gslist-of <gnc:id-type> callee-owned) tertiary))
 "set sort order.")

(gw:wrap-function
 ws
 'gnc:query-set-sort-increasing
 '<gw:void>
 "xaccQuerySetSortIncreasing"
 '((<gnc:Query*> q) 
   (<gw:bool> prim-increasing) 
   (<gw:bool> sec-increasing)
   (<gw:bool> tert-increasing))
 "sort in increasing rather than decreasing order.")

(gw:wrap-function
 ws
 'gnc:query-set-max-results
 '<gw:void>
 "qof_query_set_max_results"
 '((<gnc:Query*> q) (<gw:int> n))
 "Set the max number of results to be returned by a query.")

(gw:wrap-function
 ws
 'gnc:query->scm
 '<gw:scm>
 "gnc_query2scm"
 '((<gnc:Query*> q))
 "Convert a query to an scm representation.")

(gw:wrap-function
 ws
 'gnc:scm->query
 '<gnc:Query*>
 "gnc_scm2query"
 '((<gw:scm> query_scm))
 "Convert an scm representation of a query to a real query.")


;;=============
;; gnc-commodity

(gw:wrap-function
 ws
 'gnc:commodity-create
 '<gnc:commodity*>
 "gnc_commodity_new"
 '((<gnc:Book*> book)
   ((<gw:mchars> caller-owned const) fullname)
   ((<gw:mchars> caller-owned const) namespace)
   ((<gw:mchars> caller-owned const) mnemonic)
   ((<gw:mchars> caller-owned const) exchange-code)
   (<gw:int> smallest-fraction) )
 "Create a new gnc_commodity object.")

(gw:wrap-function
 ws
 'gnc:commodity-destroy
 '<gw:void>
 "gnc_commodity_destroy"
 '((<gnc:commodity*> comm))
 "Delete a gnc_commodity structure.")

(gw:wrap-function
 ws
 'gnc:commodity-get-mnemonic
 '(<gw:mchars> callee-owned const)
 "gnc_commodity_get_mnemonic"
 '((<gnc:commodity*> comm))
 "Get the mnemonic (ISO 3-letter string, ticker symbol, etc)")

(gw:wrap-function
 ws
 'gnc:commodity-get-namespace
 '(<gw:mchars> callee-owned const)
 "gnc_commodity_get_namespace"
 '((<gnc:commodity*> comm))
 "Get the mnemonic's namespace (ISO-4217, NASDAQ, NYSE, etc)")

(gw:wrap-function
 ws
 'gnc:commodity-get-printname
 '(<gw:mchars> callee-owned const)
 "gnc_commodity_get_printname"
 '((<gnc:commodity*> comm))
 "Get the currency's printable name .. 'USD (US Dollars)'")

(gw:wrap-function
 ws
 'gnc:commodity-get-fullname
 '(<gw:mchars> callee-owned const)
 "gnc_commodity_get_fullname"
 '((<gnc:commodity*> comm))
 "Get the currency's full name (US Dollars).")

(gw:wrap-function
 ws
 'gnc:commodity-get-exchange-code
 '(<gw:mchars> callee-owned const)
 "gnc_commodity_get_cusip"
 '((<gnc:commodity*> comm))
 "Get the cusip (exchange specific data, not the stock ticker)")

(gw:wrap-function
 ws
 'gnc:commodity-get-fraction
 '<gw:int>
 "gnc_commodity_get_fraction"
 '((<gnc:commodity*> comm))
 "Get the number of smallest transactional units per unit of the currency")

(gw:wrap-function
 ws
 'gnc:commodity-is-currency?
 '<gw:bool>
 "gnc_commodity_is_currency"
 '((<gnc:commodity*> comm))
 "return true if the commodity is an ISO4217 currency")

(gw:wrap-function
 ws
 'gnc:commodity-equiv?
 '<gw:bool>
 "gnc_commodity_equiv"
 '((<gnc:commodity*> comm1) (<gnc:commodity*> comm2))
 "Return true if the two commodities are equivalent.")

(gw:wrap-function
 ws
 'gnc:commodity-table-new
 '<gnc:commodity-table*>
 "gnc_commodity_table_new"
 '()
 "Return a new commodity table.");

(gw:wrap-function
 ws
 'gnc:commodity-table-lookup
 '<gnc:commodity*>
 "gnc_commodity_table_lookup"
 '((<gnc:commodity-table*> table)
   ((<gw:mchars> caller-owned const) namespace)
   ((<gw:mchars> caller-owned const) mnemonic))
 "Find a known gnc_commodity structure.")

(gw:wrap-function
 ws
 'gnc:commodity-table-find-full
 '<gnc:commodity*>
 "gnc_commodity_table_find_full"
 '((<gnc:commodity-table*> table)
   ((<gw:mchars> caller-owned const) namespace)
   ((<gw:mchars> caller-owned const) printname))
 "Find a gnc_commodity structure from its printable name.")

(gw:wrap-function
 ws
 'gnc:commodity-table-insert
 '<gnc:commodity*>
 "gnc_commodity_table_insert"
 '((<gnc:commodity-table*> table)
   (<gnc:commodity*> commodity))
 "Add a commodity to the table.")

(gw:wrap-function
 ws
 'gnc:commodity-table-has-namespace
 '<gw:bool>
 "gnc_commodity_table_has_namespace"
 '((<gnc:commodity-table*> table)
   ((<gw:mchars> caller-owned const) namespace))
 "Predicate to test for existence of a namespace.")

(gw:wrap-function
 ws
 'gnc:commodity-table-get-namespaces
 '(gw:glist-of (<gw:mchars> callee-owned) callee-owned)
 "gnc_commodity_table_get_namespaces"
 '((<gnc:commodity-table*> table))
 "Return a list of all the namespaces in the table.")

(gw:wrap-function
 ws
 'gnc:commodity-table-add-namespace
 '<gnc:commodity-namespace*>
 "gnc_commodity_table_add_namespace"
 '((<gnc:commodity-table*> table)
   ((<gw:mchars> caller-owned const) namespace)
   (<gnc:Book*> book))
 "Add a new namespace to the commodity table")

(gw:wrap-function
 ws
 'gnc:commodity-table-delete-namespace
 '<gw:void>
 "gnc_commodity_table_delete_namespace"
 '((<gnc:commodity-table*> table)
   ((<gw:mchars> caller-owned const) namespace))
 "Delete a namespace from the commodity table")

(gw:wrap-function
 ws
 'gnc:commodity-table-get-commodities
 '(gw:glist-of <gnc:commodity*> caller-owned)
 "gnc_commodity_table_get_commodities"
 '((<gnc:commodity-table*> table)
   ((<gw:mchars> caller-owned const) namespace))
 "Return a list of all the commodities in a given namespace in the table.")

(gw:wrap-function
 ws
 'gnc:commodity-table-get-quotable-commodities
 '(gw:glist-of <gnc:commodity*> caller-owned)
 "gnc_commodity_table_get_quotable_commodities"
 '((<gnc:commodity-table*> table))
 "Return a list of all the quotable commodities in a given namespace in the table.")

(gw:wrap-function
 ws
 'gnc:commodity-table-add-default-data
 '<gw:bool>
 "gnc_commodity_table_add_default_data"
 '((<gnc:commodity-table*> table)
   (<gnc:Book*> book))
 "Add default commodities to the commodity table.")

;;=========

(gw:wrap-function
 ws
 'gnc:engine-shutdown
 '<gw:void>
 "gnc_engine_shutdown"
 '()
 "Shutdown the gnucash engine.")

;;============
;; gnc_numeric

(gw:wrap-function
 ws
 'gnc:numeric-create
 '<gnc:numeric>
 "gnc_numeric_create"
 '((<gw:gint64> num) (<gw:gint64> denom))
 "Create a new gnc_numeric object")

(gw:wrap-function
 ws
 'gnc:numeric-zero
 '<gnc:numeric>
 "gnc_numeric_zero"
 '()
 "Create a zero-valued gnc_numeric")

(gw:wrap-function
 ws
 'gnc:numeric-error
 '<gnc:numeric>
 "gnc_numeric_error"
 '((<gw:int> code))
 "Create an error-signaling gnc_numeric")

(gw:wrap-function
 ws
 'gnc:numeric-check
 '<gw:int>
 "gnc_numeric_check"
 '((<gnc:numeric> val))
 "Check for error code in a gnc_numeric")

(gw:wrap-function
 ws
 'gnc:numeric-num
 '<gw:gint64>
 "gnc_numeric_num"
 '((<gnc:numeric> arg))
 "Return the numerator of a gnc_numeric")

(gw:wrap-function
 ws
 'gnc:numeric-denom
 '<gw:gint64>
 "gnc_numeric_denom"
 '((<gnc:numeric> arg))
 "Return the denominator of a gnc_numeric")

(gw:wrap-function
 ws
 'gnc:numeric-zero-p
 '<gw:bool>
 "gnc_numeric_zero_p"
 '((<gnc:numeric> arg))
 "Check for zero.")

(gw:wrap-function
 ws
 'gnc:numeric-compare
 '<gw:int>
 "gnc_numeric_compare"
 '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
 "Compare 2 gnc_numeric (1 if a>b, 0 if a==b, -1 if a<b)")

(gw:wrap-function
 ws
 'gnc:numeric-negative-p
 '<gw:bool>
 "gnc_numeric_negative_p"
 '((<gnc:numeric> arg))
 "Check for arg < 0")

(gw:wrap-function
 ws
 'gnc:numeric-positive-p
 '<gw:bool>
 "gnc_numeric_positive_p"
 '((<gnc:numeric> arg))
 "Check for arg > 0")

(gw:wrap-function
 ws
 'gnc:numeric-eq
 '<gw:bool>
 "gnc_numeric_eq"
 '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
 "Check for arg1 exactly arg2")

(gw:wrap-function
 ws
 'gnc:numeric-equal
 '<gw:bool>
 "gnc_numeric_equal"
 '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
 "Check for arg1 same number as arg2")

(gw:wrap-function
 ws
 'gnc:numeric-same
 '<gw:bool>
 "gnc_numeric_same"
 '((<gnc:numeric> arg1)
   (<gnc:numeric> arg2)
   (<gw:gint64> denom)
   (<gw:int> how))
 "Check for arg1 same number as arg2 if converted to common denom")

(gw:wrap-function
 ws
 'gnc:numeric-add
 '<gnc:numeric>
 "gnc_numeric_add"
 '((<gnc:numeric> arg1)
   (<gnc:numeric> arg2)
   (<gw:gint64> denom)
   (<gw:int> how))
 "Return a+b")

(gw:wrap-function
 ws
 'gnc:numeric-sub
 '<gnc:numeric>
 "gnc_numeric_sub"
 '((<gnc:numeric> arg1)
   (<gnc:numeric> arg2)
   (<gw:gint64> denom)
   (<gw:int> how))
 "Return a-b")

(gw:wrap-function
 ws
 'gnc:numeric-mul
 '<gnc:numeric>
 "gnc_numeric_mul"
 '((<gnc:numeric> arg1)
   (<gnc:numeric> arg2)
   (<gw:gint64> denom)
   (<gw:int> how))
 "Return a*b")

(gw:wrap-function
 ws
 'gnc:numeric-div
 '<gnc:numeric>
 "gnc_numeric_div"
 '((<gnc:numeric> arg1)
   (<gnc:numeric> arg2)
   (<gw:gint64> denom)
   (<gw:int> how))
 "Return a/b")

(gw:wrap-function
 ws
 'gnc:numeric-neg
 '<gnc:numeric>
 "gnc_numeric_neg"
 '((<gnc:numeric> arg))
 "Return -a")

(gw:wrap-function
 ws
 'gnc:numeric-abs
 '<gnc:numeric>
 "gnc_numeric_abs"
 '((<gnc:numeric> arg))
 "Return |a|")

(gw:wrap-function
 ws
 'gnc:numeric-add-fixed
 '<gnc:numeric>
 "gnc_numeric_add_fixed"
 '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
 "Return a+b under strict fixed-denom rules")

(gw:wrap-function
 ws
 'gnc:numeric-sub-fixed
 '<gnc:numeric>
 "gnc_numeric_sub_fixed"
 '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
 "Return a-b under strict fixed-denom rules")

(gw:wrap-function
 ws
 'gnc:numeric-convert
 '<gnc:numeric>
 "gnc_numeric_convert"
 '((<gnc:numeric> arg) (<gw:gint64> denom) (<gw:int> how))
 "Convert a gnc_numeric to a new denominator")

(gw:wrap-function
 ws
 'gnc:double-to-gnc-numeric
 '<gnc:numeric>
 "double_to_gnc_numeric"
 '((<gw:double> in) (<gw:gint64> denom) (<gw:int> how))
 "Convert <gw:double> to gnc_numeric")

(gw:wrap-function
 ws
 'gnc:numeric-to-double
 '<gw:double>
 "gnc_numeric_to_double"
 '((<gnc:numeric> in))
 "Convert gnc_numeric to double")

(gw:wrap-function
 ws
 'gnc:numeric-to-string
 '(<gw:mchars> caller-owned const)
 "gnc_numeric_to_string"
 '((<gnc:numeric> arg))
 "Convert gnc_numeric to a printable string")

;; gnc-date.h

(gw:wrap-function
 ws
 'gnc:timepair-canonical-day-time
 '<gnc:time-pair>
 "timespecCanonicalDayTime"
 '((<gnc:time-pair> tp))
 "Convert a timepair on a certain day (localtime) to\
the timepair representing midday on that day")

;;
;; gnc-filepath-utils.h
;;

(gw:wrap-function
 ws
 'gnc:build-dotgnucash-path
 '(<gw:mchars> caller-owned)
 "gnc_build_dotgnucash_path"
 '(((<gw:mchars> caller-owned) filename))
 "Convert a relative path name into a full path name in the .gnucash directory")

(gw:wrap-function
 ws
 'gnc:build-book-path
 '(<gw:mchars> caller-owned)
 "gnc_build_book_path"
 '(((<gw:mchars> caller-owned) filename))
 "Convert a relative path name into a full path name in the .gnucash/books directory")

;;
;; gnc-lot.h
;;

(gw:wrap-function
 ws
 'gnc:lot-get-balance
 '<gnc:numeric>
 "gnc_lot_get_balance"
 '((<gnc:Lot*> lot))
 "Return the balance of the lot")

(gw:wrap-function
 ws
 'gnc:lot-closed?
 '<gw:bool>
 "gnc_lot_is_closed"
 '((<gnc:Lot*> lot))
 "Is this Lot closed (is the balance zero)?")

(gw:wrap-function
 ws
 'gnc:lot-get-splits
 '(gw:glist-of <gnc:Split*> callee-owned)
 "gnc_lot_get_split_list"
 '((<gnc:Lot*> lot))
 "Return the list of splits attached to this lot.")

(gw:wrap-function
 ws
 'qof:event-suspend
 '<gw:void>
 "qof_event_suspend"
 '()
 "Suspend all engine events.") 

(gw:wrap-function
 ws
 'qof:event-resume
 '<gw:void>
 "qof_event_resume"
 '()
 "Resume engine event generation.") 

(gw:wrap-function
 ws
 'gnc:quote-source-set-fq-installed
 '<gw:void>
 "gnc_quote_source_set_fq_installed"
 '(((gw:glist-of (<gw:mchars> callee-owned) callee-owned) choices))
 "Takes a list of installed Finance::Quote souces and records it internally.")


;; Budget functions

(gw:wrap-as-wct ws '<gnc:Budget*> "GncBudget *" "const GncBudget *")

(gw:wrap-function
 ws
 'gnc:budget-get-guid
 '<gnc:guid-scm>
 "gnc_budget_return_guid"
 '((<gnc:Budget*> budget))
 "Gets the guid of the budget")


(gw:wrap-function
 ws
 'gnc:budget-lookup
 '<gnc:Budget*>
 "gnc_budget_lookup_direct"
 '((<gnc:guid-scm> guid)
   (<gnc:Book*> book))
 "Lookup a budget from its GUID.")


(gw:wrap-function
 ws
 'gnc:budget-get-default
 '<gnc:Budget*>
 "gnc_budget_get_default"
 '((<gnc:Book*> book))
 "Get the default budget for the book.")


(gw:wrap-function
 ws
 'gnc:budget-get-name
 '(<gw:mchars> callee-owned const)
 "gnc_budget_get_name"
 '((<gnc:Budget*> budget))
 "Get the brief name for the budget.")

(gw:wrap-function
 ws
 'gnc:budget-get-num-periods
 '<gw:unsigned-int>
 "gnc_budget_get_num_periods"
 '((<gnc:Budget*> budget))
 "Get the number of periods in a budget.")

(gw:wrap-function
 ws
 'gnc:budget-is-account-period-value-set
 '<gw:bool>
 "gnc_budget_is_account_period_value_set"
 '((<gnc:Budget*> budget)
   (<gnc:Account*> acct)
   (<gw:unsigned-int> period_num)
   )
 "Determine if the given account and budget period has a budgeted value.")

(gw:wrap-function
 ws
 'gnc:budget-get-account-period-value
 '<gnc:numeric>
 "gnc_budget_get_account_period_value"
 '((<gnc:Budget*> budget)
   (<gnc:Account*> acct)
   (<gw:unsigned-int> period_num)
   )
 "Get the budgeted value for the given account and budget period.")

(gw:wrap-function
 ws
 'gnc:budget-get-account-period-actual-value
 '<gnc:numeric>
 "gnc_budget_get_account_period_actual_value"
 '((<gnc:Budget*> budget)
   (<gnc:Account*> acct)
   (<gw:unsigned-int> period_num)
   )
 "Get the actual account value for the given account and budget period.")

(gw:wrap-function
 ws
 'gnc:budget-get-period-start-date
 '<gnc:time-pair>
 "gnc_budget_get_period_start_date"
 '((<gnc:Budget*> budget)
   (<gw:unsigned-int> period_num)
   )
 "Get the date that the given period begins.")

;;
;; gnc-hooks-scm.h
;;   (and gnc-hooks.h)
;;
(gw:wrap-function
 ws
 'gnc:hook-define
 '(<gw:mchars> caller-owned)
 "gnc_hook_create"
 '(((<gw:mchars> caller-owned) name) (<gw:int> how) ((<gw:mchars> caller-owned) desc))
 "Define (create) a new hook")

(gw:wrap-function
 ws
 'gnc:hook-get-description
 '(<gw:mchars> callee-owned)
 "gnc_hook_get_description"
 '(((<gw:mchars> caller-owned) hook))
 "Get the description of a hook")

(gw:wrap-function
 ws
 'gnc:hook-add-dangler
 '<gw:void>
 "gnc_hook_add_scm_dangler"
 '(((<gw:mchars> caller-owned) hook) (<gw:scm> procedure))
 "Add a hook dangler to an existing hook")

(gw:wrap-function
 ws
 'gnc:hook-remove-dangler
 '<gw:void>
 "gnc_hook_del_scm_dangler"
 '(((<gw:mchars> caller-owned) hook) (<gw:scm> procedure))
 "Remove a hook dangler from an existing hook")

(gw:wrap-function
 ws
 'gnc:hook-run-danglers-real
 '<gw:void>
 "gnc_hook_run"
 '(((<gw:mchars> caller-owned) name) (<gnc:Session*> arg))
 "Run the danglers on a hook.")

; Now wrap all the 'known' hooks
(gw:wrap-value ws 'gnc:*new-book-hook*
	       '(<gw:mchars> callee-owned) "HOOK_NEW_BOOK")
(gw:wrap-value ws 'gnc:*report-hook*
	       '(<gw:mchars> callee-owned) "HOOK_REPORT")
(gw:wrap-value ws 'gnc:*save-options-hook*
	       '(<gw:mchars> callee-owned) "HOOK_SAVE_OPTIONS")
(gw:wrap-value ws 'gnc:*book-opened-hook*
	       '(<gw:mchars> callee-owned) "HOOK_BOOK_OPENED")
(gw:wrap-value ws 'gnc:*book-closed-hook*
	       '(<gw:mchars> callee-owned) "HOOK_BOOK_CLOSED")
