;;; -*-scheme-*-

(define-module (g-wrapped gw-engine-spec))

(use-modules (g-wrap))
(use-modules (g-wrapped gw-glib-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(let ((mod (gw:new-module "gw-engine")))
  (define (standard-c-call-gen result func-call-code)
    (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
  
  (define (add-standard-result-handlers! type c->scm-converter)
    (define (standard-pre-handler result)
      (let* ((ret-type-name (gw:result-get-proper-c-type-name result))
             (ret-var-name (gw:result-get-c-name result)))
        (list "{\n"
              "    " ret-type-name " " ret-var-name ";\n")))
    
    (gw:type-set-pre-call-result-ccodegen! type standard-pre-handler)
    
    (gw:type-set-post-call-result-ccodegen!
     type
     (lambda (result)
       (let* ((scm-name (gw:result-get-scm-name result))
              (c-name (gw:result-get-c-name result)))
         (list
          (c->scm-converter scm-name c-name)
          "  }\n")))))
  
  (gw:module-depends-on mod "gw-runtime")
  (gw:module-depends-on mod "gw-glib")
  (gw:module-set-guile-module! mod '(g-wrapped gw-engine))

  ;; Timespec
  (let ((wt (gw:wrap-type mod '<gnc:time-pair> "Timespec" "const Timespec")))
    
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt (lambda (param)
          (list "gnc_timepair_p(" (gw:param-get-scm-name param) ")")))
    
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_timepair2timespec(" x ")"))))
         (list c-name " = " (old-func scm-name) ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)
    
    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_timespec2timepair(" x ")"))))
         (list scm-name " = " (old-func c-name) ";\n")))))

  ;; GUID 
  (let ((wt (gw:wrap-type mod '<gnc:guid-scm> "GUID" "const GUID")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x) (list "gnc_guid_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))

    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param)))
         (list c-name " = gnc_scm2guid(" scm-name ");\n"))))

    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (list scm-name " = gnc_guid2scm(" c-name ");\n"))))
  
  ;; have to specially wrap a list of gnc_commodities
  (let ((wt (gw:wrap-type
             mod
             '<gnc:list-of-commodity*>
             "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_commodity_ptr_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_commodity_ptr(" x ")"))))
         (list c-name
               " = "
               (old-func scm-name)
               ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_commodity_ptr_to_scm(" x ")"))))
         (list scm-name
               " = "
               (old-func c-name)
               ";\n")))))

  ;; price list 
  (let ((wt 
         (gw:wrap-type mod '<gnc:list-of-price*> "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_price_ptr_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_price_ptr(" x ")"))))
         (list c-name
               " = "
               (old-func scm-name)
               ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_price_ptr_to_scm(" x ")"))))
         (list scm-name
               " = "
               (old-func c-name)
               ";\n")))))

  ;; list of split *
  (let ((wt (gw:wrap-type mod '<gnc:list-of-split*-callee-owned>
                          "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_split_ptr_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_split_ptr(" x ")"))))
         (list c-name " = " (old-func scm-name) ";\n"))))

    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_split_ptr_to_scm_no_free(" x ")"))))
         (list scm-name " = " (old-func c-name) ";\n")))))

  ;; list of transaction *
  (let ((wt (gw:wrap-type mod '<gnc:list-of-transaction*-callee-owned>
                          "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_transaction_ptr_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_transaction_ptr(" x ")"))))
         (list c-name " = " (old-func scm-name) ";\n"))))

    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_transaction_ptr_to_scm_no_free(" x ")"))))
         (list scm-name " = " (old-func c-name) ";\n")))))

  ;; list of account * 
  (let ((wt (gw:wrap-type mod '<gnc:list-of-account*-caller-owned>
                          "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_account_ptr_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_account_ptr(" x ")"))))
         (list c-name
               " = "
               (old-func scm-name)
               ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_account_ptr_to_scm(" x ")"))))
         (list scm-name
               " = "
               (old-func c-name)
               ";\n")))))

  (let ((wt (gw:wrap-type mod '<gnc:list-of-account*-callee-owned>
                          "GList *" "const GList *")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_account_ptr_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x)
                 (list "gnc_scm_to_glist_account_ptr(" x ")"))))
         (list c-name " = " (old-func scm-name) ";\n"))))

    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x)
                (list "gnc_glist_account_ptr_to_scm_no_free(" x ")"))))
         (list scm-name " = " (old-func c-name) ";\n")))))

  ;; gnc-numeric
  (let ((wt 
         (gw:wrap-type mod '<gnc:numeric> "gnc_numeric" "const gnc_numeric")))
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (let ((old-func
              (lambda (x) (list "gnc_numeric_p(" x ")"))))
         (old-func (gw:param-get-scm-name param)))))
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (let* ((scm-name (gw:param-get-scm-name param))
              (c-name (gw:param-get-c-name param))
              (old-func
               (lambda (x) (list "gnc_scm_to_numeric(" x ")"))))
         (list c-name " = " (old-func scm-name) ";\n"))))
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func
              (lambda (x) (list "gnc_numeric_to_scm(" x ")"))))
         (list scm-name " = " (old-func c-name) ";\n")))))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <glib.h>\n"
      "#include <guid.h>\n"
      "#include <Query.h>\n"
      "#include <Backend.h>\n"
      "#include <Group.h>\n"
      "#include <gnc-book.h>\n"
      "#include <gnc-book-p.h>\n"
      "#include <gnc-session.h>\n"
      "#include <gnc-engine-util.h>\n"
      "#include <date.h>\n"
      "#include <engine-helpers.h>\n"
      "#include <gnc-engine.h>\n"
      "#include <gnc-commodity.h>\n"
      "#include <gnc-numeric.h>\n")))

  (gw:module-set-init-ccodegen!
   mod
   (lambda (client-only?) 
     (if client-only? 
         '()
         (gw:inline-scheme '(use-modules (gnucash engine))))))
  
  (gw:wrap-non-native-type mod '<gnc:Account*> "Account*" "const Account*")
  (gw:wrap-non-native-type mod '<gnc:Account**> "Account**" "const Account**")

  (gw:wrap-non-native-type mod '<gnc:InvAcct*> "InvAcct*" "const InvAcct*")
  (gw:wrap-non-native-type mod '<gnc:AccInfo*> "AccInfo*" "const AccInfo*")
  (gw:wrap-non-native-type mod '<gnc:AccountGroup*> 
                           "AccountGroup*" "const AccountGroup*")
  (gw:wrap-non-native-type mod '<gnc:Book*> "GNCBook*" "const GNCBook*")
  (gw:wrap-non-native-type mod '<gnc:Session*>
                           "GNCSession*" "const GNCSession**")

  (gw:wrap-non-native-type mod '<gnc:Split*> "Split*" "const Split*")
  (gw:wrap-non-native-type mod '<gnc:Transaction*> 
                           "Transaction*" "const Transaction*")
  
  (gw:wrap-non-native-type mod '<gnc:commodity*> 
                           "gnc_commodity*" "const gnc_commodity*")
  (gw:wrap-non-native-type mod '<gnc:commodity-table*> "gnc_commodity_table*" 
                           "const gnc_commodity_table*")

  (gw:wrap-non-native-type mod '<gnc:Query*> "Query *" "const Query *")
  (gw:wrap-non-native-type mod '<gnc:QueryTerm*> 
                           "QueryTerm *" "const QueryTerm *")

  (let ((wt (gw:wrap-enumeration mod '<gnc:query-op> 
				 "QueryOp" "const QueryOp")))
    (gw:enum-add-value! wt "QUERY_AND" 'query-and)
    (gw:enum-add-value! wt "QUERY_OR" 'query-or)
    (gw:enum-add-value! wt "QUERY_NAND" 'query-nand)
    (gw:enum-add-value! wt "QUERY_NOR" 'query-nor)
    (gw:enum-add-value! wt "QUERY_XOR" 'query-xor))

  (let ((wt (gw:wrap-enumeration mod '<gnc:sort-type>
                                 "sort_type_t" "const sort_type_t")))
    (gw:enum-add-value! wt "BY_STANDARD" 'by-standard)
    (gw:enum-add-value! wt "BY_DATE" 'by-date)
    (gw:enum-add-value! wt "BY_DATE_ROUNDED" ' by-date-rounded)
    (gw:enum-add-value! wt "BY_DATE_ENTERED" 'by-date-entered)
    (gw:enum-add-value! wt "BY_DATE_ENTERED_ROUNDED" 'by-date-entered-rounded)
    (gw:enum-add-value! wt "BY_DATE_RECONCILED" 'by-date-reconciled)
    (gw:enum-add-value! wt "BY_DATE_RECONCILED_ROUNDED" 
			'by-date-reconciled-rounded)
    (gw:enum-add-value! wt "BY_NUM" 'by-num)
    (gw:enum-add-value! wt "BY_AMOUNT" 'by-amount)
    (gw:enum-add-value! wt "BY_MEMO" 'by-memo)
    (gw:enum-add-value! wt "BY_DESC" 'by-desc)
    (gw:enum-add-value! wt "BY_RECONCILE" 'by-reconcile)
    (gw:enum-add-value! wt "BY_ACCOUNT_FULL_NAME" 'by-account-full-name)
    (gw:enum-add-value! wt "BY_ACCOUNT_CODE" 'by-account-code)
    (gw:enum-add-value! wt "BY_CORR_ACCOUNT_FULL_NAME" 'by-corr-account-full-name)
    (gw:enum-add-value! wt "BY_CORR_ACCOUNT_CODE" 'by-corr-account-code)
    (gw:enum-add-value! wt "BY_NONE" 'by-none)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:query-term-type>
                                 "pd_type_t" "const pd_type_t")))
    (gw:enum-add-value! wt "PD_ACCOUNT" 'pd-account)
    (gw:enum-add-value! wt "PD_AMOUNT" 'pd-amount)
    (gw:enum-add-value! wt "PD_BALANCE" 'pd-balance)
    (gw:enum-add-value! wt "PD_CLEARED" 'pd-cleared)
    (gw:enum-add-value! wt "PD_DATE" 'pd-date)
    (gw:enum-add-value! wt "PD_GUID" 'pd-guid)
    (gw:enum-add-value! wt "PD_KVP" 'pd-kvp)
    (gw:enum-add-value! wt "PD_MISC" 'pd-misc)
    (gw:enum-add-value! wt "PD_STRING" 'pd-string)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:query-pred-type>
                                 "pr_type_t" "const pr_type_t")))
    (gw:enum-add-value! wt "PR_ACCOUNT" 'pr-account)
    (gw:enum-add-value! wt "PR_ACTION" 'pr-action)
    (gw:enum-add-value! wt "PR_AMOUNT" 'pr-amount)
    (gw:enum-add-value! wt "PR_BALANCE" 'pr-balance)
    (gw:enum-add-value! wt "PR_CLEARED" 'pr-cleared)
    (gw:enum-add-value! wt "PR_DATE" 'pr-date)
    (gw:enum-add-value! wt "PR_DESC" 'pr-desc)
    (gw:enum-add-value! wt "PR_GUID" 'pr-guid)
    (gw:enum-add-value! wt "PR_KVP" 'pr-kvp)
    (gw:enum-add-value! wt "PR_MEMO" 'pr-memo)
    (gw:enum-add-value! wt "PR_NUM" 'pr-num)
    (gw:enum-add-value! wt "PR_PRICE" 'pr-price)
    (gw:enum-add-value! wt "PR_SHRS" 'pr-shares)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:acct-match-how>
                                 "acct_match_t" "const acct_match_t")))
    (gw:enum-add-value! wt "ACCT_MATCH_ALL" 'acct-match-all)
    (gw:enum-add-value! wt "ACCT_MATCH_ANY" 'acct-match-any)
    (gw:enum-add-value! wt "ACCT_MATCH_NONE" 'acct-match-none))

  (let ((wt (gw:wrap-enumeration mod '<gnc:amt-match-how>
                                 "amt_match_t" "const amt_match_t")))
    (gw:enum-add-value! wt "AMT_MATCH_ATLEAST" 'amt-match-atleast)
    (gw:enum-add-value! wt "AMT_MATCH_ATMOST" 'amt-match-atmost)
    (gw:enum-add-value! wt "AMT_MATCH_EXACTLY" 'amt-match-exactly)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:amt-match-sign>
                                 "amt_match_sgn_t" "const amt_match_sgn_t")))
    (gw:enum-add-value! wt "AMT_SGN_MATCH_EITHER" 'amt-sgn-match-either)
    (gw:enum-add-value! wt "AMT_SGN_MATCH_CREDIT" 'amt-sgn-match-credit)
    (gw:enum-add-value! wt "AMT_SGN_MATCH_DEBIT" 'amt-sgn-match-debit)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:cleared-match-how>
                                 "cleared_match_t" "const cleared_match_t")))
    (gw:enum-add-value! wt "CLEARED_NO" 'cleared-match-no)
    (gw:enum-add-value! wt "CLEARED_CLEARED" 'cleared-match-cleared)
    (gw:enum-add-value! wt "CLEARED_RECONCILED" 'cleared-match-reconciled)
    (gw:enum-add-value! wt "CLEARED_FROZEN" 'cleared-match-frozen)
    (gw:enum-add-value! wt "CLEARED_VOIDED" 'cleared-match-voided)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:balance-match-how>
                                 "balance_match_t" "const balance_match_t")))
    (gw:enum-add-value! wt "BALANCE_BALANCED" 'balance-match-balanced)
    (gw:enum-add-value! wt "BALANCE_UNBALANCED" 'balance-match-unbalanced)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:query-run-t>
                                 "query_run_t" "const query_run_t")))
    (gw:enum-add-value! wt "QUERY_MATCH_ALL" 'query-match-all)    
    (gw:enum-add-value! wt "QUERY_MATCH_ANY" 'query-match-any)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:kvp-match-how>
                                 "kvp_match_t" "const kvp_match_t")))
    (gw:enum-add-value! wt "KVP_MATCH_LT" 'kvp-match-lt)
    (gw:enum-add-value! wt "KVP_MATCH_LTE" 'kvp-match-lte)
    (gw:enum-add-value! wt "KVP_MATCH_EQ" 'kvp-match-eq)
    (gw:enum-add-value! wt "KVP_MATCH_GTE" 'kvp-match-gte)
    (gw:enum-add-value! wt "KVP_MATCH_GT" 'kvp-match-gt)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:kvp-match-where>
                                 "kvp_match_where_t"
                                 "const kvp_match_where_t")))
    (gw:enum-add-value! wt "KVP_MATCH_SPLIT" 'kvp-match-split)
    (gw:enum-add-value! wt "KVP_MATCH_TRANS" 'kvp-match-trans)
    (gw:enum-add-value! wt "KVP_MATCH_ACCOUNT" 'kvp-match-account)
    #t)

  (let ((wt (gw:wrap-enumeration mod '<gnc:kvp-value-t>
                                 "kvp_value_t" "const kvp_value_t")))
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

  (let ((we 
         (gw:wrap-enumeration mod '<gnc:AccountType> 
                              "GNCAccountType" "const GNCAccountType")))
    ;; From Account.h
    (gw:enum-add-value! we "BAD_TYPE" 'bad-type)
    (gw:enum-add-value! we "NO_TYPE" 'no-type)
    (gw:enum-add-value! we "BANK" 'bank)
    (gw:enum-add-value! we "CASH" 'cash)
    (gw:enum-add-value! we "CREDIT" 'credit)
    (gw:enum-add-value! we "ASSET" 'asset)
    (gw:enum-add-value! we "LIABILITY" 'liability)
    (gw:enum-add-value! we "STOCK" 'stock)
    (gw:enum-add-value! we "MUTUAL" 'mutual-fund)
    (gw:enum-add-value! we "CURRENCY" 'currency)
    (gw:enum-add-value! we "INCOME" 'income)
    (gw:enum-add-value! we "EXPENSE" 'expense)
    (gw:enum-add-value! we "EQUITY" 'equity)
    (gw:enum-add-value! we "NUM_ACCOUNT_TYPES" 'num-account-types)
    (gw:enum-add-value! we "CHECKING" 'checking)
    (gw:enum-add-value! we "SAVINGS" 'savings)
    (gw:enum-add-value! we "MONEYMRKT" 'money-market)
    (gw:enum-add-value! we "CREDITLINE" 'credit-line)
    #t)

  (let ((we (gw:wrap-enumeration mod
                                 '<gnc:BackendError>
                                 "GNCBackendError" "const GNCBackendError")))

    (gw:enum-add-value! we "ERR_BACKEND_NO_ERR" 'no-err)
    (gw:enum-add-value! we "ERR_BACKEND_NO_BACKEND" 'no-backend)
    (gw:enum-add-value! we "ERR_BACKEND_BAD_URL" 'bad-url)
    (gw:enum-add-value! we "ERR_BACKEND_CANT_CONNECT" 'cant-connect)
    (gw:enum-add-value! we "ERR_BACKEND_CONN_LOST" 'connection-lost)
    (gw:enum-add-value! we "ERR_BACKEND_NO_SUCH_DB" 'no-such-db)
    (gw:enum-add-value! we "ERR_BACKEND_LOCKED" 'locked)
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

;;     (gw:enum-add-value! we "ERR_RPC_HOST_UNK" 'rpc_host_unk)
;;     (gw:enum-add-value! we "ERR_RPC_CANT_BIND" 'rpc_cant_bind)
;;     (gw:enum-add-value! we "ERR_RPC_CANT_ACCEPT" 'rpc_cant_accept)
;;     (gw:enum-add-value! we "ERR_RPC_NO_CONNECTION" 'rpc_no_connection)
;;     (gw:enum-add-value! we "ERR_RPC_BAD_VERSION" 'rpc_bad_version)
;;     (gw:enum-add-value! we "ERR_RPC_FAILED" 'rpc_failed)
;;     (gw:enum-add-value! we "ERR_RPC_NOT_ADDED" 'rpc_not_added)
    #t)

  (gw:wrap-function
   mod
   'gnc:guid-new
   '<gnc:guid-scm>
   "guid_new_return"
   '()
   "Return a newly-generated GUID.")

  (gw:wrap-function
   mod
   'gnc:split-get-guid
   '<gnc:guid-scm>
   "xaccSplitReturnGUID"
   '((<gnc:Split*> s))
   "Return the GUID of Split s.")

  (gw:wrap-function
   mod
   'gnc:split-get-balance
   '<gnc:numeric>
   "xaccSplitGetBalance"
   '((<gnc:Split*> s))
   "Return balance at split.")

  (gw:wrap-function
   mod
   'gnc:split-get-memo
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccSplitGetMemo"
   '((<gnc:Split*> s))
   "Return split's memo.")

  (gw:wrap-function
   mod
   'gnc:split-get-action
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccSplitGetAction"
   '((<gnc:Split*> s))
   "Return split's action.")

  (gw:wrap-function
   mod
   'gnc:split-get-reconcile-state
   '<gw:char>
   "xaccSplitGetReconcile"
   '((<gnc:Split*> s))
   "Return split's reconcile state.")

  (gw:wrap-function
   mod
   'gnc:split-get-reconciled-date
   '<gnc:time-pair>
   "gnc_split_get_date_reconciled"
   '((<gnc:Split*> s))
   "Return split's reconciliation date.")

  (gw:wrap-function
   mod
   'gnc:split-get-amount
   '<gnc:numeric>
   "xaccSplitGetAmount"
   '((<gnc:Split*> s))
   "Return split's amount.")

  (gw:wrap-function
   mod
   'gnc:split-get-share-price
   '<gnc:numeric>
   "xaccSplitGetSharePrice"
   '((<gnc:Split*> s))
   "Return split's share price.")

  (gw:wrap-function
   mod
   'gnc:split-get-value
   '<gnc:numeric>
   "xaccSplitGetValue"
   '((<gnc:Split*> s))
   "Return split's value.")

  (gw:wrap-function
   mod
   'gnc:split-get-account
   '<gnc:Account*>
   "xaccSplitGetAccount"
   '((<gnc:Split*> s))
   "Return split's account.")

  (gw:wrap-function
   mod
   'gnc:split-get-parent
   '<gnc:Transaction*>
   "xaccSplitGetParent"
   '((<gnc:Split*> s))
   "Return the parent transaction of the split.")

  (gw:wrap-function
   mod
   'gnc:split-get-other-split
   '<gnc:Split*>
   "xaccSplitGetOtherSplit"
   '((<gnc:Split*> s))
   "Return the 'other' split of the parent transaction or NULL.")

  (gw:wrap-function
   mod
   'gnc:split-compare-account-full-names
   '<gw:int>
   "xaccSplitCompareAccountFullNames"
   '((<gnc:Split*> sa) (<gnc:Split*> sb))
   "Compare two splits on the full names of their parent accounts")
  
  (gw:wrap-function
   mod
   'gnc:split-compare-account-codes
   '<gw:int>
   "xaccSplitCompareAccountCodes"
   '((<gnc:Split*> sa) (<gnc:Split*> sb))
   "Compare two splits on the codes of their parent accounts")

  (gw:wrap-function
   mod
   'gnc:split-compare-other-account-full-names
   '<gw:int>
   "xaccSplitCompareOtherAccountFullNames"
   '((<gnc:Split*> sa) (<gnc:Split*> sb))
   "Compare two splits on the full names of the *other* 
split in the transaction")

  (gw:wrap-function
   mod
   'gnc:split-compare-other-account-codes
   '<gw:int>
   "xaccSplitCompareOtherAccountCodes"
   '((<gnc:Split*> sa) (<gnc:Split*> sb))
   "Compare two splits on the codes of the *other* split in the transaction")

  (gw:wrap-function
   mod
   'gnc:split-get-corr-account-name
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccSplitGetCorrAccountName"
   '((<gnc:Split*> sa))
   "Find the split on the other side of the transaction, and return the name of
its account")

  (gw:wrap-function
   mod
   'gnc:split-get-corr-account-full-name-internal
   '<glib:g-chars-caller-owned>
   "xaccSplitGetCorrAccountFullName"
   '((<gnc:Split*> sa) (<gw:char> separator))
   "Find the split on the other side of the transaction, and return the 
name of its account.  Don't use directly, use 
gnc:split-get-corr-account-full-name in src/scm/report-utilities.scm")
  (gw:wrap-function
   mod
   'gnc:split-get-corr-account-code
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccSplitGetCorrAccountCode" 
   '((<gnc:Split*> sa))
   "Find the split on the other side of the transaction, and return the 
code of its account")

  (gw:wrap-function
   mod
   'gnc:transaction-get-guid
   '<gnc:guid-scm>
   "xaccTransReturnGUID"
   '((<gnc:Transaction*> t))
   "Return the GUID of Transaction t.")

  (gw:wrap-function
   mod
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
   mod
   'gnc:transaction-get-splits
   '<gnc:list-of-split*-callee-owned>
   "xaccTransGetSplitList"
   '((<gnc:Transaction*> t))
   "Returns a list of the splits in t.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-num
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccTransGetNum"
   '((<gnc:Transaction*> t))
   "Return the transaction's num, an arbitrary user-assigned field.  It
is intended to store a short id number, typically the check number,
deposit number, invoice number or other tracking number.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-description
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccTransGetDescription"
   '((<gnc:Transaction*> t))
   "Return the transaction description, an arbitrary user-assigned
value.  It is meant to be a short descriptive phrase.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-notes
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccTransGetNotes"
   '((<gnc:Transaction*> t))
   "Return the transaction notes field.")

  (gw:wrap-function
   mod
   'gnc:transaction-void
   '(<gw:void>)
   "xaccTransVoid"
   '((<gnc:Transaction*> transaction)
     ((<gw:m-chars-caller-owned> gw:const) reason))
   "Void a transaction")

  (gw:wrap-function
   mod
   'gnc:transaction-get-void-status
   '<gw:bool>
   "xaccTransGetVoidStatus"
   '((<gnc:Transaction*> transaction))
   "Return true if a transaction has been voided")

  (gw:wrap-function
   mod
   'gnc:transaction-get-void-reason
   '<gw:m-chars-callee-owned>
   "xaccTransGetVoidReason"
   '((<gnc:Transaction*> transaction))
   "return a string indicating reason for voiding")
   
  (gw:wrap-function
   mod
   'gnc:split-void-former-amount
   '<gnc:numeric>
   "xaccSplitVoidFormerAmount"
   '((<gnc:Split*> split))
   "get what the split's amount before voiding")

  (gw:wrap-function
   mod
   'gnc:split-void-former-value
   '<gnc:numeric>
   "xaccSplitVoidFormerValue"
   '((<gnc:Split*> split))
   "get what the split's value was before voiding")

  (gw:wrap-function
   mod
   'gnc:dmy2timespec
   '<gnc:time-pair>
   "gnc_dmy2timespec"
   '((<gw:int> day) (<gw:int> month) (<gw:int> year))
   "Return a given day, month, and year as a pair where the car is the
number of seconds and the cdr is the number of nanoseconds.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-date-posted
   '<gnc:time-pair>
   "gnc_transaction_get_date_posted"
   '((<gnc:Transaction*> t))
   "Return the date the transaction was posted at the bank as a pair of
integers.  The car is the number of seconds and the cdr is the number
of nanoseconds.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-date-entered
   '<gnc:time-pair>
   "gnc_transaction_get_date_entered"
   '((<gnc:Transaction*> t))
   "Return the date the transaction was entered into the register as a
pair of integers.  The car is the number of seconds and the cdr is the
number of nanoseconds.")

  (gw:wrap-function
   mod
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
   mod
   'gnc:transaction-set-date-entered
   '<gw:void>
   "gnc_transaction_set_date_entered"
   '((<gnc:Transaction*> t) (<gnc:time-pair> date))
   "Modifies entered date of the transaction. The date given should be a
pair of integers.  The car is the number of seconds and the cdr is the
number of nanoseconds.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-split-count
   '<gw:int>
   "xaccTransCountSplits"
   '((<gnc:Transaction*> t))
   "Return the number of splits in the transaction.")

  (gw:wrap-function
   mod
   'gnc:transaction-get-currency
   '<gnc:commodity*>
   "xaccTransGetCurrency"
   '((<gnc:Transaction*> trans))
   "Returns the commodity common for this transaction. ATM it gives the same result as xaccTransFindCommonCurrency.")

  (gw:wrap-function
   mod
   'gnc:transaction-set-currency
   '<gw:void>
   "xaccTransSetCurrency"
   '((<gnc:Transaction*> trans) (<gnc:commodity*> comm))
   "Sets the commodity common for this transaction.")

  (gw:wrap-function
   mod
   'gnc:malloc-account
   '<gnc:Account*>
   "xaccMallocAccount"
   '((<gnc:Book*> book))
   "Allocate a new account structure.")

  (gw:wrap-function
   mod
   'gnc:account-destroy
   '<gw:void>
   "xaccAccountDestroy"
   '((<gnc:Account*> account))
   "Free an account structure. (Must call gnc:account-begin-edit first)")

  (let ((docstr
         "The gnc:account-begin-edit and gnc:account-commit-edit
 subroutines provide a two-phase-commit wrapper for account updates."))
    (gw:wrap-function
     mod
     'gnc:account-begin-edit
     '<gw:void>
     "xaccAccountBeginEdit"
     '((<gnc:Account*> a))
     docstr)
    (gw:wrap-function
     mod
     'gnc:account-commit-edit
     '<gw:void>
     "xaccAccountCommitEdit"
     '((<gnc:Account*> a))
     docstr))

  (gw:wrap-function
   mod
   'gnc:account-insert-split
   '<gw:void>
   "xaccAccountInsertSplit"
   '((<gnc:Account*> a) (<gnc:Split*> s))
   "Insert the split s into account a. If the split already belongs
to another account, it will be removed from that account first.")

  (gw:wrap-function
   mod
   'gnc:account-fix-split-date-order
   '<gw:void>
   "xaccAccountFixSplitDateOrder"
   '((<gnc:Account*> a) (<gnc:Split*> s))
   "Check to see if split s is in proper sorted date order with respect
to the other splits in account a.")

  (gw:wrap-function
   mod
   'gnc:trans-fix-split-date-order
   '<gw:void>
   "xaccTransFixSplitDateOrder"
   '((<gnc:Transaction*> t))
   "Check to see if all of the splits in transaction t are in proper
date order.")

  (gw:wrap-function
   mod
   'gnc:account-order
   '<gw:int>
   "xaccAccountOrder"
   '((<gnc:Account**> a1) (<gnc:Account**> a2))
   "Defines a sorting order on accounts.  Returns -1 if a1 is \"less
than\" the second, +1 if the a1 is \"greater than\" the second, and 0
if they are equal.  To determine the sort order, the account codes are
compared, and if these are equal, then account types, and, if these
are equal, then account names.")

  (gw:wrap-function
   mod
   'gnc:account-set-type
   '<gw:void>
   "xaccAccountSetType"
   '((<gnc:Account*> a) (<gnc:AccountType> type))
   "Set Account type.  See definition of scheme GNCAccountType for values.")

  (gw:wrap-function
   mod
   'gnc:account-set-name
   '<gw:void>
   "xaccAccountSetName"
   '((<gnc:Account*> a) ((<gw:m-chars-caller-owned> gw:const) name))
   "Set account name")

  (gw:wrap-function
   mod
   'gnc:account-set-code
   '<gw:void>
   "xaccAccountSetCode"
   '((<gnc:Account*> a) ((<gw:m-chars-caller-owned> gw:const) code))
   "Set whatever cryptic code we wish to use as the alternative to
the account name.

Commonly these codes are numeric, and organize asset accounts together
in one number range, liability accounts in another number range, and
so forth...")

  (gw:wrap-function
   mod
   'gnc:account-set-description
   '<gw:void>
   "xaccAccountSetDescription"
   '((<gnc:Account*> a) ((<gw:m-chars-caller-owned> gw:const) description))
   "Set a slightly-more-verbose description for the account.")

  (gw:wrap-function
   mod
   'gnc:account-set-notes
   '<gw:void>
   "xaccAccountSetNotes"
   '((<gnc:Account*> a) ((<gw:m-chars-caller-owned> gw:const) notes))
   "Set up a longer set of notes that provide a perhaps-very-verbose
description of the nature of a particular account.")

  (gw:wrap-function
   mod
   'gnc:account-set-tax-related
   '<gw:void>
   "xaccAccountSetTaxRelated"
   '((<gnc:Account*> a) (<gw:bool> tax-related))
   "Set the tax-related flag of the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-guid
   '<gnc:guid-scm>
   "xaccAccountReturnGUID"
   '((<gnc:Account*> a))
   "Get the GUID of Account a.")

  (gw:wrap-function
   mod
   'gnc:account-lookup
   '<gnc:Account*>
   "xaccAccountLookupDirect"
   '((<gnc:guid-scm> guid) (<gnc:Book*> book))
   "Lookup the account with GUID guid.")

  (gw:wrap-function
   mod
   'gnc:account-get-type-string
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetTypeStr"
   '((<gnc:AccountType> type))
   "What's the account type's name.")

  (gw:wrap-function
   mod
   'gnc:account-get-type
   '<gnc:AccountType>
   "xaccAccountGetType"
   '((<gnc:Account*> a))
   "What's the account type?  See scheme GNCAccountType enum def values.")

  (gw:wrap-function
   mod
   'gnc:account-get-name
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetName"
   '((<gnc:Account*> a))
   "Get the brief name for the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-code
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetCode"
   '((<gnc:Account*> a))
   "Get the account's ``account code.''  

Commonly this is used to provide a hierarchy where accounts with
similar classifications (e.g. - Assets, Liabilities, Equity, Income,
Expenses) are given numeric codes in corresponding ``number ranges.''")

  (gw:wrap-function
   mod
   'gnc:account-get-description
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetDescription"
   '((<gnc:Account*> a))
   "Get the slightly-verbose description of the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-notes
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetNotes"
   '((<gnc:Account*> a))
   "Get the fully-verbose description of the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-commodity
   '(<gnc:commodity*> gw:const)
   "xaccAccountGetCommodity"
   '((<gnc:Account*> a))
   "Get the commodity in which the account is denominated.")

  (gw:wrap-function
   mod
   'gnc:account-set-commodity
   '<gw:void>
   "xaccAccountSetCommodity"
   '((<gnc:Account*> a) (<gnc:commodity*> comm))
   "Set the commodity in which the account is denominated.")

  (gw:wrap-function
   mod
   'gnc:account-get-tax-related
   '<gw:bool>
   "xaccAccountGetTaxRelated"
   '((<gnc:Account*> a))
   "Get the tax related flag of the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-tax-US-code
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetTaxUSCode"
   '((<gnc:Account*> a))
   "Get the tax code set on the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-tax-US-payer-name-source
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetTaxUSPayerNameSource"
   '((<gnc:Account*> a))
   "Get the tax payer name source set on the account.")

  (gw:wrap-function
   mod
   'gnc:account-get-price-src
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetPriceSrc"
   '((<gnc:Account*> a))
   "Get the account's price source, if any.")

  (gw:wrap-function
   mod
   'gnc:account-get-quote-tz
   '(<gw:m-chars-callee-owned> gw:const)
   "xaccAccountGetQuoteTZ"
   '((<gnc:Account*> a))
   "Get the quote source's timezone, if any.")

  (gw:wrap-function
   mod
   'gnc:account-get-children
   '<gnc:AccountGroup*>
   "xaccAccountGetChildren"
   '((<gnc:Account*> a))
   "Get a pointer to an AccountGroup that represents the set of
children to this account.")

  (gw:wrap-function
   mod
   'gnc:account-get-parent
   '<gnc:AccountGroup*>
   "xaccAccountGetParent"
   '((<gnc:Account*> a))
   "Get the pointer to the account's parent.")

  (gw:wrap-function
   mod
   'gnc:account-get-parent-account
   '<gnc:Account*>
   "xaccAccountGetParentAccount"
   '((<gnc:Account*> a))
   "Get the pointer to the account's parent account.")

  (gw:wrap-function
   mod
   'gnc:account-get-balance
   '<gnc:numeric>
   "xaccAccountGetBalance"
   '((<gnc:Account*> a))
   "Undocumented.")

  (gw:wrap-function
   mod
   'gnc:account-get-cleared-balance
   '<gnc:numeric>
   "xaccAccountGetClearedBalance"
   '((<gnc:Account*> a))
   "Undocumented.")

  (gw:wrap-function
   mod
   'gnc:account-get-reconciled-balance
   '<gnc:numeric>
   "xaccAccountGetReconciledBalance"
   '((<gnc:Account*> a))
   "Undocumented.")

  (gw:wrap-function
   mod
   'gnc:account-get-split-list
   '<glib:GList*>
   "xaccAccountGetSplitList"
   '((<gnc:Account*> a))
   "Get the GList of splits in account a.")

  (gw:wrap-function
   mod
   'gnc:malloc-account-group
   '<gnc:AccountGroup*>
   "xaccMallocAccountGroup"
   '((<gnc:Book*> book))
   "Create a new account group.")

  (gw:wrap-function
   mod
   'gnc:free-account-group
   '<gw:void>
   "xaccFreeAccountGroup"
   '((<gnc:AccountGroup*> g))
   "Free an account group.")

  (gw:wrap-function
   mod
   'gnc:group-merge-accounts
   '<gw:void>
   "xaccGroupMergeAccounts"
   '((<gnc:AccountGroup*> g))
   "Merge accounts which have the same name and description. Used in
importing Quicken files.")

  (gw:wrap-function
   mod
   'gnc:group-concat-group
   '<gw:void>
   "xaccGroupConcatGroup"
   '((<gnc:AccountGroup*> old)
     (<gnc:AccountGroup*> new))
   "Catenate accounts from one group into another. Used in Quicken
import.")

  (gw:wrap-function
   mod
   'gnc:group-get-num-subaccounts
   '<gw:int>
   "xaccGroupGetNumSubAccounts"
   '((<gnc:AccountGroup*> g))
   "Return the number of accounts, including subaccounts, in the account
group")

  (gw:wrap-function
   mod
   'gnc:group-get-num-accounts
   '<gw:int>
   "xaccGroupGetNumAccounts"
   '((<gnc:AccountGroup*> g))
   "Return the number of accounts in the indicated group only"
   "(children not counted).")
  
  (gw:wrap-function
   mod
   'gnc:group-get-account
   '<gnc:Account*>
   "xaccGroupGetAccount"
   '((<gnc:AccountGroup*> g) (<gw:int> n))
   "Return account number n in account group g.")

  (gw:wrap-function
   mod
   'gnc:get-account-from-full-name
   '<gnc:Account*>
   "xaccGetAccountFromFullName"
   '((<gnc:AccountGroup*> g)
     ((<gw:m-chars-caller-owned> gw:const) name)
     (<gw:char> separator))
   "Return account named name in group g.  full path with separators.")

  (gw:wrap-function
   mod
   'gnc:group-get-parent
   '<gnc:Account*>
   "xaccGroupGetParentAccount"
   '((<gnc:AccountGroup*> g))
   "Return the parent acount for the group.")

  (gw:wrap-function
   mod
   'gnc:group-insert-account
   '<gw:void>
   "xaccGroupInsertAccount"
   '((<gnc:AccountGroup*> g) (<gnc:Account*> a))
   "Add account a to group g.")

  (gw:wrap-function
   mod
   'gnc:account-insert-subaccount
   '<gw:void>
   "xaccAccountInsertSubAccount"
   '((<gnc:Account*> p) (<gnc:Account*> c))
   "Add a child account c to parent p")

  (gw:wrap-function
   mod
   'gnc:group-get-subaccounts
   '<gnc:list-of-account*-caller-owned>
   "xaccGroupGetSubAccounts"
   '((<gnc:AccountGroup*> g))
   "Return a GList containing all of the accounts, including
subaccounts, in the account group. The returned array should be freed
when no longer needed.")

  (gw:wrap-function
   mod
   'gnc:group-get-account-list
   '<gnc:list-of-account*-callee-owned>
   "xaccGroupGetAccountList"
   '((<gnc:AccountGroup*> g))
   "Return a GList containing the immediate children of g.")

  (gw:wrap-function
   mod
   'gnc:group-begin-staged-transaction-traversals
   '<gw:void>
   "xaccGroupBeginStagedTransactionTraversals"
   '((<gnc:AccountGroup*> group))
   "Sets things up to begin a sequence of staged traversals.")

  (gw:wrap-function
   mod
   'gnc:group-staged-transaction-traversal
   '<gw:bool>
   "gnc_scmGroupStagedTransactionTraversal"
   '((<gnc:AccountGroup*> group)
     (<gw:unsigned-int> stage)
     (<gw:scm> thunk))
   "FIXME: For now, see Group.h for info...")

  (gw:wrap-function
   mod
   'gnc:account-staged-transaction-traversal
   '<gw:bool>
   "gnc_scmAccountStagedTransactionTraversal"
   '((<gnc:Account*> account)
     (<gw:unsigned-int> stage)
     (<gw:scm> thunk))
   "FIXME: For now, see Group.h for info...")

  ;;============
  ;; GNCPriceDB

  (gw:wrap-non-native-type mod '<gnc:PriceDB*>
                           "GNCPriceDB *" "const GNCPriceDB *")
  (gw:wrap-non-native-type mod '<gnc:Price*>
                           "GNCPrice *" "const GNCPrice *")
  
  (gw:wrap-function
   mod
   'gnc:price-create
   '<gnc:Price*>
   "gnc_price_create"
   '((<gnc:Book*> book))
   "Create and return a new price.")

  (gw:wrap-function
   mod
   'gnc:price-unref
   '<gw:void>
   "gnc_price_unref"
   '((<gnc:Price*> p))
   "Indicate you're finished with this price.")

  (gw:wrap-function
   mod
   'gnc:price-set-commodity
   '<gw:void>
   "gnc_price_set_commodity"
   '((<gnc:Price*> p) (<gnc:commodity*> c))
   "Set the price's commodity.")

  (gw:wrap-function
   mod
   'gnc:price-set-currency
   '<gw:void>
   "gnc_price_set_currency"
   '((<gnc:Price*> p) (<gnc:commodity*> c))
   "Set the price's currency.")

  (gw:wrap-function
   mod
   'gnc:price-set-time
   '<gw:void>
   "gnc_price_set_time"
   '((<gnc:Price*> p) (<gnc:time-pair> t))
   "Set the price's time stamp.")

  (gw:wrap-function
   mod
   'gnc:price-set-source
   '<gw:void>
   "gnc_price_set_source"
   '((<gnc:Price*> p) (<gw:m-chars-caller-owned> src))
   "Set the price's source.")

  (gw:wrap-function
   mod
   'gnc:price-set-type
   '<gw:void>
   "gnc_price_set_type"
   '((<gnc:Price*> p) (<gw:m-chars-caller-owned> type))
   "Set the price's type.")

  (gw:wrap-function
   mod
   'gnc:price-set-value
   '<gw:void>
   "gnc_price_set_value"
   '((<gnc:Price*> p) (<gnc:numeric> value))
   "Set the price's value.")

  (gw:wrap-function
   mod
   'gnc:price-get-value
   '<gnc:numeric>
   "gnc_price_get_value"
   '((<gnc:Price*> p))
   "Get the price's value")

  (gw:wrap-function
   mod
   'gnc:price-get-commodity
   '<gnc:commodity*> 
   "gnc_price_get_commodity"
   '((<gnc:Price*> p))
   "Get the commodity this price is for.")

  (gw:wrap-function
   mod
   'gnc:price-get-currency
   '<gnc:commodity*> 
   "gnc_price_get_currency"
   '((<gnc:Price*> p))
   "Get the currency (commodity) this price's value is denominated in.")

  (gw:wrap-function
   mod
   'gnc:price-get-time
   '<gnc:time-pair>
   "gnc_price_get_time"
   '((<gnc:Price*> p))
   "Get the time stamp of this price.")

  (gw:wrap-function
   mod
   'gnc:pricedb-add-price
   '<gw:bool>
   "gnc_pricedb_add_price"
   '((<gnc:PriceDB*> db) (<gnc:Price*> p))
   "Add a price to the DB.  Unref the price when you're finished with it.")

  (gw:wrap-function
   mod
   'gnc:pricedb-lookup-latest
   '<gnc:Price*>
   "gnc_pricedb_lookup_latest"
   '((<gnc:PriceDB*> db)
     (<gnc:commodity*> commodity) (<gnc:commodity*> currency))
   "Returns the latest price.  Unref the price when you're finished with it.")

  (gw:wrap-function
   mod
   'gnc:pricedb-lookup-nearest-in-time
   '<gnc:Price*>
   "gnc_pricedb_lookup_nearest_in_time"
   '((<gnc:PriceDB*> db)
     (<gnc:commodity*> commodity) (<gnc:commodity*> currency)
     (<gnc:time-pair> t))
   "Returns the price quote nearest to t.  Unref price when finished with it.")

  (gw:wrap-function
   mod
   'gnc:pricedb-get-prices
   '<gnc:list-of-price*>
   "gnc_pricedb_get_prices"
   '((<gnc:PriceDB*> db)
     (<gnc:commodity*> commodity)
     (<gnc:commodity*> currency))
   "Get all prices for commodity in currency.")

  (gw:wrap-function
   mod
   'gnc:pricedb-lookup-at-time
   '<gnc:list-of-price*>
   "gnc_pricedb_lookup_at_time"
   '((<gnc:PriceDB*> db)
     (<gnc:commodity*> commodity) (<gnc:commodity*> currency)
     (<gnc:time-pair> t))
   "Lookup a price at time t.")

  ;;===========
  ;; GNCSession

  (gw:wrap-function
   mod
   'gnc:session-new
   '<gnc:Session*>
   "gnc_session_new" '()
   "Create a new session.")

  (gw:wrap-function
   mod
   'gnc:session-destroy
   '<gw:void>
   "gnc_session_destroy"
   '((<gnc:Session*> session))
   "Destroy the given session.")

  (gw:wrap-function
   mod
   'gnc:session-get-book
   '<gnc:Book*>
   "gnc_session_get_book"
   '((<gnc:Session*> session))
   "Get the book of the given session.")

  (gw:wrap-function
   mod
   'gnc:session-begin
   '<gw:bool>
   "gnc_session_begin"
   '((<gnc:Session*> session)
     ((<gw:m-chars-caller-owned> gw:const) id)
     (<gw:bool> ignore-lock?)
     (<gw:bool> create-if-nonexistent?))
   "Setup the session for use.")

  (gw:wrap-function
   mod
   'gnc:session-load
   '<gw:bool>
   "gnc_session_load"
   '((<gnc:Session*> session))
   "Load the data associated with the given session.")

  (gw:wrap-function
   mod
   'gnc:session-save
   '<gw:void>
   "gnc_session_save"
   '((<gnc:Session*> session))
   "Save the data in the session.")

  (gw:wrap-function
   mod
   'gnc:session-end
   '<gw:void>
   "gnc_session_end"
   '((<gnc:Session*> session))
   "Indicate you're finished with the session.")

  (gw:wrap-function
   mod
   'gnc:book-get-group
   '<gnc:AccountGroup*>
   "gnc_book_get_group"
   '((<gnc:Book*> book))
   "Get the book's account group.")

  ;; XXX FIXME this is a private function, should not be exported publically.
  (gw:wrap-function
   mod
   'gnc:book-set-group
   '<gw:void>
   "gnc_book_set_group"
   '((<gnc:Book*> book) (<gnc:AccountGroup*> group))
   "Set the book's account group.")

  (gw:wrap-function
   mod
   'gnc:book-get-commodity-table
   '<gnc:commodity-table*>
   "gnc_book_get_commodity_table"
   '((<gnc:Book*> book))
   "Get the book's commodity table.")

  (gw:wrap-function
   mod
   'gnc:book-get-pricedb
   '<gnc:PriceDB*>
   "gnc_book_get_pricedb"
   '((<gnc:Book*> book))
   "Get the book's pricedb.")

  (gw:wrap-function
   mod
   'gnc:session-get-error
   '<gnc:BackendError>
   "gnc_session_get_error"
   '((<gnc:Session*> session))
   "Check for a pending error.")

  (gw:wrap-function
   mod
   'gnc:session-pop-error
   '<gnc:BackendError>
   "gnc_session_pop_error"
   '((<gnc:Session*> session))
   "Remove an error, if any, from the error stack.")

  (gw:wrap-function
   mod
   'gnc:set-log-level-global
   '<gw:void>
   "gnc_set_log_level_global"
   '((<gw:int> level))
   "Set the logging level for all modules to level.")

  (gw:wrap-function
   mod
   'gnc:print-date
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_print_date"
   '((<gnc:time-pair> date))
   "Returns a string with the date formatted according to the
current settings")

  (gw:wrap-function
   mod
   'gnc:transaction-destroy
   '<gw:void>
   "xaccTransDestroy"
   '((<gnc:Transaction*> t))
   "Destroys the transaction in question.")

  (gw:wrap-function
   mod
   'gnc:transaction-begin-edit
   '<gw:void>
   "xaccTransBeginEdit"
   '((<gnc:Transaction*> t))
   "Start an edit session on a transaction.")

  (gw:wrap-function
   mod
   'gnc:transaction-commit-edit
   '<gw:void>
   "xaccTransCommitEdit"
   '((<gnc:Transaction*> t))
   "Commit edits to a transaction.")

  (gw:wrap-function
   mod
   'gnc:transaction-is-open
   '<gw:bool>
   "xaccTransIsOpen"
   '((<gnc:Transaction*> t))
   "Returns true if the transaction t is open for editing.")

  (gw:wrap-function
   mod
   'gnc:split-destroy
   '<gw:void>
   "xaccSplitDestroy"
   '((<gnc:Split*> s))
   "Destroys the split in question.  Probably only useful inside the context
of having a parent transaction with which one is working...")

  (gw:wrap-function
   mod
   'gnc:transaction-append-split
   '<gw:void>
   "xaccTransAppendSplit"
   '((<gnc:Transaction*> t) (<gnc:Split*> s))
   "Adds a split to a transaction.")

  (gw:wrap-function
   mod
   'gnc:transaction-set-date
   '<gw:void>
   "xaccTransSetDate"
   '((<gnc:Transaction*> t)
     (<gw:int> day)
     (<gw:int> month)
     (<gw:int> year))
   "Set date on transaction based on day, month, year values")

  (gw:wrap-function
   mod
   'gnc:transaction-set-date-time-pair
   '<gw:void>
   "gnc_transaction_set_date"
   '((<gnc:Transaction*> t) (<gnc:time-pair> date))
   "Set date on transaction based on the time-pair")

  (gw:wrap-function
   mod
   'gnc:transaction-set-xnum
   '<gw:void>
   "xaccTransSetNum"
   '((<gnc:Transaction*> t) ((<gw:m-chars-caller-owned> gw:const) xnum))
   "Set the XNUM - e.g. - cheque number or other identifier")

  (gw:wrap-function
   mod
   'gnc:transaction-set-description
   '<gw:void>
   "xaccTransSetDescription"
   '((<gnc:Transaction*> t) ((<gw:m-chars-caller-owned> gw:const) desc))
   "Set the transaction description.")

  (gw:wrap-function
   mod
   'gnc:transaction-set-notes
   '<gw:void>
   "xaccTransSetNotes"
   '((<gnc:Transaction*> t) ((<gw:m-chars-caller-owned> gw:const) notes))
   "Set the transaction notes field.")

  (gw:wrap-function
   mod
   'gnc:transaction-create
   '<gnc:Transaction*>
   "xaccMallocTransaction"
   '((<gnc:Book*> book>))
   "Create a Transaction structure")

  (gw:wrap-function
   mod
   'gnc:split-create
   '<gnc:Split*>
   "xaccMallocSplit"
   '((<gnc:Book*> book>>))
   "Create a Split structure")

  (gw:wrap-function
   mod
   'gnc:split-set-value
   '<gw:void>
   "xaccSplitSetValue"
   '((<gnc:Split*> s) (<gnc:numeric> amount))
   "Set value for split")

  (gw:wrap-function
   mod
   'gnc:split-set-base-value
   '<gw:void>
   "xaccSplitSetBaseValue"
   '((<gnc:Split*> s)
     (<gnc:numeric> amount)
     (<gnc:commodity*> currency))
   "Set value for split with currency")

  (gw:wrap-function
   mod
   'gnc:split-set-memo
   '<gw:void>
   "xaccSplitSetMemo"
   '((<gnc:Split*> s) ((<gw:m-chars-caller-owned> gw:const) memo))
   "Set memo for a split")

  (gw:wrap-function
   mod
   'gnc:split-set-action
   '<gw:void>
   "xaccSplitSetAction"
   '((<gnc:Split*> s) ((<gw:m-chars-caller-owned> gw:const) action))
   "Set Action for a split.")

  (gw:wrap-function
   mod
   'gnc:split-set-reconcile
   '<gw:void>
   "xaccSplitSetReconcile"
   '((<gnc:Split*> s) (<gw:char> value))
   "Set reconcile state for split entry")

  (gw:wrap-function
   mod
   'd-gnc:split-set-share-price
   '<gw:void>
   "DxaccSplitSetSharePrice"
   '((<gnc:Split*> s) (<gw:double> value))
   "Set share price for split entry")

  (gw:wrap-function
   mod
   'gnc:split-set-share-price
   '<gw:void>
   "xaccSplitSetSharePrice"
   '((<gnc:Split*> s) (<gnc:numeric> value))
   "Set share price for split entry")

  (gw:wrap-function
   mod
   'gnc:split-set-amount
   '<gw:void>
   "xaccSplitSetAmount"
   '((<gnc:Split*> s) (<gnc:numeric> value))
   "Set amount for split entry")

  (gw:wrap-function
   mod
   'gnc:split-set-share-price-and-amount
   '<gw:void>
   "xaccSplitSetSharePriceAndAmount"
   '((<gnc:Split*> s)
     (<gnc:numeric> price)
     (<gnc:numeric> amount))
   "Set the share price and amount for split entry")

  (gw:wrap-function
   mod
   'gnc:malloc-query
   '<gnc:Query*>
   "xaccMallocQuery"
   '()
   "Create a new (empty) Query structure.")

  (gw:wrap-function
   mod
   'gnc:free-query
   '<gw:void>
   "xaccFreeQuery"
   '((<gnc:Query*> q))
   "Delete the Query and its terms.")

  (gw:wrap-function
   mod
   'gnc:query-set-group
   '<gw:void>
   "xaccQuerySetGroup"
   '((<gnc:Query*> q) (<gnc:AccountGroup*> group))
   "Set the account-group that a query pertains to.")

  (gw:wrap-function
   mod
   'gnc:query-invert
   '<gnc:Query*>
   "xaccQueryInvert"
   '((<gnc:Query*> q))
   "Logically invert a Query (returns a newly-allocated Query object)")

  (gw:wrap-function
   mod
   'gnc:query-merge
   '<gnc:Query*>
   "xaccQueryMerge"
   '((<gnc:Query*> q1) (<gnc:Query*> q2) (<gnc:query-op> qop))
   "Merge two queries (returns a newly allocated object")

  (gw:wrap-function
   mod
   'gnc:query-clear
   '<gw:void>
   "xaccQueryClear"
   '((<gnc:Query*> q))
   "Clear the terms from a query object")

  (gw:wrap-function
   mod
   'gnc:query-purge-terms
   '<gw:void>
   "xaccQueryPurgeTerms"
   '((<gnc:Query*> q) (<gnc:query-term-type> tt))
   "Remove query terms of a particular type.")

  (gw:wrap-function
   mod
   'gnc:query-has-terms?
   '<gw:bool>
   "xaccQueryHasTerms"
   '((<gnc:Query*> q))
   "Check if a Query is clear or has terms.")

  (gw:wrap-function
   mod
   'gnc:query-get-splits
   '<glib:GList*>
   "xaccQueryGetSplits"
   '((<gnc:Query*> q))
   "Return a list of splits matching the Query.")

  (gw:wrap-function
   mod
   'gnc:query-get-splits-unique-trans
   '<glib:GList*>
   "xaccQueryGetSplitsUniqueTrans"
   '((<gnc:Query*> q))
   "Return a list of splits matching the Query, but at most one per transaction")

  (gw:wrap-function
   mod
   'gnc:query-get-transactions
   '<glib:GList*>
   "xaccQueryGetTransactions"
   '((<gnc:Query*> q) (<gnc:query-run-t> rt))
   "Find transactions with splits matching the Query.")

  (gw:wrap-function
   mod
   'gnc:query-add-account-match
   '<gw:void>
   "xaccQueryAddAccountMatch"
   '((<gnc:Query*> q)
     (<glib:GList*> accts)
     (<gnc:acct-match-how> acctmatch)
     (<gnc:query-op> how))
   "Match one or all of a set of accounts.")

  (gw:wrap-function
   mod
   'gnc:query-add-single-account-match
   '<gw:void>
   "xaccQueryAddSingleAccountMatch"
   '((<gnc:Query*> q) (<gnc:Account*> acct) (<gnc:query-op> how))
   "Match a single account.")

  (gw:wrap-function
   mod
   'gnc:query-add-description-match
   '<gw:void>
   "xaccQueryAddDescriptionMatch"
   '((<gnc:Query*> q)
     ((<gw:m-chars-caller-owned> gw:const) mstring)
     (<gw:int> case-sens)
     (<gw:int> use-regexp)
     (<gnc:query-op> how))
   "Match transaction description.")

  (gw:wrap-function
   mod
   'gnc:query-add-number-match
   '<gw:void>
   "xaccQueryAddNumberMatch"
   '((<gnc:Query*> q)
     ((<gw:m-chars-caller-owned> gw:const) mstring)
     (<gw:int> case-sens)
     (<gw:int> use-regexp)
     (<gnc:query-op> how))
   "Match the Number field.")

  (gw:wrap-function
   mod
   'gnc:query-add-action-match
   '<gw:void>
   "xaccQueryAddActionMatch"
   '((<gnc:Query*> q)
     ((<gw:m-chars-caller-owned> gw:const) mstring)
     (<gw:int> case-sens)
     (<gw:int> use-regexp)
     (<gnc:query-op> how))
   "Match the Action field (a string).")

  (gw:wrap-function
   mod
   'd-gnc:query-add-amount-match
   '<gw:void>
   "DxaccQueryAddAmountMatch"
   '((<gnc:Query*> q)
     (<gw:double> amount)
     (<gnc:amt-match-sign> sgn)
     (<gnc:amt-match-how> mhow)
     (<gnc:query-op> how))
   "Match the amount (including inequalities)")

  (gw:wrap-function
   mod
   'd-gnc:query-add-share-price-match
   '<gw:void>
   "DxaccQueryAddSharePriceMatch"
   '((<gnc:Query*> q)
     (<gw:double> amount)
     (<gnc:amt-match-how> mhow)
     (<gnc:query-op> how))
   "Match the share price (including inequalities)")

  (gw:wrap-function
   mod
   'd-gnc:query-add-shares-match
   '<gw:void>
   "DxaccQueryAddSharesMatch"
   '((<gnc:Query*> q)
     (<gw:double> amount)
     (<gnc:amt-match-how> mhow)
     (<gnc:query-op> how))
   "Match the share price (ncluding inequalities)")

  (gw:wrap-function
   mod
   'gnc:query-add-date-match
   '<gw:void>
   "xaccQueryAddDateMatch"
   '((<gnc:Query*> q)
     (<gw:bool> use-start)
     (<gw:int> st-year)
     (<gw:int> st-mon)
     (<gw:int> st-day)
     (<gw:bool> use-end)
     (<gw:int> end-year)
     (<gw:int> end-mon)
     (<gw:int> end-day)
     (<gnc:query-op> how))
   "Match the transaction date.")

  (gw:wrap-function
   mod
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
   mod
   'gnc:query-add-memo-match
   '<gw:void>
   "xaccQueryAddMemoMatch"
   '((<gnc:Query*> q)
     ((<gw:m-chars-caller-owned> gw:const) mstring)
     (<gw:int> case-sens)
     (<gw:int> use-regexp)
     (<gnc:query-op> how))
   "Match the Number field.")

  (gw:wrap-function
   mod
   'gnc:query-add-cleared-match
   '<gw:void>
   "xaccQueryAddClearedMatch"
   '((<gnc:Query*> q) (<gnc:cleared-match-how> cleared-how) (<gnc:query-op> how))
   "match the cleared state.")

  (gw:wrap-function
   mod
   'gnc:query-set-sort-order
   '<gw:void>
   "xaccQuerySetSortOrder"
   '((<gnc:Query*> q)
     (<gnc:sort-type> primary)
     (<gnc:sort-type> secondary)
     (<gnc:sort-type> tertiary))
   "set sort order.")

  (gw:wrap-function
   mod
   'gnc:query-set-sort-increasing
   '<gw:void>
   "xaccQuerySetSortIncreasing"
   '((<gnc:Query*> q) 
     (<gw:bool> prim-increasing) 
     (<gw:bool> sec-increasing)
     (<gw:bool> tert-increasing))
   "sort in increasing rather than decreasing order.")

  (gw:wrap-function
   mod
   'gnc:query-set-max-splits
   '<gw:void>
   "xaccQuerySetMaxSplits"
   '((<gnc:Query*> q) (<gw:int> n))
   "Set the max number of splits to be returned by a query.")

  (gw:wrap-function
   mod
   'gnc:query->scm
   '<gw:scm>
   "gnc_query2scm"
   '((<gnc:Query*> q))
   "Convert a query to an scm representation.")

  (gw:wrap-function
   mod
   'gnc:scm->query
   '<gnc:Query*>
   "gnc_scm2query"
   '((<gw:scm> query_scm))
   "Convert an scm representation of a query to a real query.")


  ;;=============
  ;; gnc-commodity

  (gw:wrap-function
   mod
   'gnc:commodity-create
   '<gnc:commodity*>
   "gnc_commodity_new"
   '(((<gw:m-chars-caller-owned> gw:const) fullname)
     ((<gw:m-chars-caller-owned> gw:const) namespace)
     ((<gw:m-chars-caller-owned> gw:const) mnemonic)
     ((<gw:m-chars-caller-owned> gw:const) exchange-code)
     (<gw:int> smallest-fraction))
   "Create a new gnc_commodity object.")

  (gw:wrap-function
   mod
   'gnc:commodity-destroy
   '<gw:void>
   "gnc_commodity_destroy"
   '((<gnc:commodity*> comm))
   "Delete a gnc_commodity structure.")

  (gw:wrap-function
   mod
   'gnc:commodity-get-mnemonic
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_commodity_get_mnemonic"
   '((<gnc:commodity*> comm))
   "Get the mnemonic (ISO 3-letter string, ticker symbol, etc)")

  (gw:wrap-function
   mod
   'gnc:commodity-get-namespace
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_commodity_get_namespace"
   '((<gnc:commodity*> comm))
   "Get the mnemonic's namespace (ISO-4217, NASDAQ, NYSE, etc)")

  (gw:wrap-function
   mod
   'gnc:commodity-get-printname
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_commodity_get_printname"
   '((<gnc:commodity*> comm))
   "Get the currency's printable name .. 'USD (US Dollars)'")

  (gw:wrap-function
   mod
   'gnc:commodity-get-fullname
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_commodity_get_fullname"
   '((<gnc:commodity*> comm))
   "Get the currency's full name (US Dollars).")

  (gw:wrap-function
   mod
   'gnc:commodity-get-exchange-code
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_commodity_get_exchange_code"
   '((<gnc:commodity*> comm))
   "Get the exchange code (ISO numeric code)")

  (gw:wrap-function
   mod
   'gnc:commodity-get-fraction
   '<gw:int>
   "gnc_commodity_get_fraction"
   '((<gnc:commodity*> comm))
   "Get the number of smallest transactional units per unit of the currency")

  (gw:wrap-function
   mod
   'gnc:commodity-equiv?
   '<gw:bool>
   "gnc_commodity_equiv"
   '((<gnc:commodity*> comm1) (<gnc:commodity*> comm2))
   "Return true if the two commodities are equivalent.")

  (gw:wrap-function
   mod
   'gnc:commodity-table-new
   '<gnc:commodity-table*>
   "gnc_commodity_table_new"
   '()
   "Return a new commodity table.");

  (gw:wrap-function
   mod
   'gnc:commodity-table-lookup
   '<gnc:commodity*>
   "gnc_commodity_table_lookup"
   '((<gnc:commodity-table*> table)
     ((<gw:m-chars-caller-owned> gw:const) namespace)
     ((<gw:m-chars-caller-owned> gw:const) mnemonic))
   "Find a known gnc_commodity structure.")

  (gw:wrap-function
   mod
   'gnc:commodity-table-find-full
   '<gnc:commodity*>
   "gnc_commodity_table_find_full"
   '((<gnc:commodity-table*> table)
     ((<gw:m-chars-caller-owned> gw:const) namespace)
     ((<gw:m-chars-caller-owned> gw:const) printname))
   "Find a gnc_commodity structure from its printable name.")

  (gw:wrap-function
   mod
   'gnc:commodity-table-insert
   '<gnc:commodity*>
   "gnc_commodity_table_insert"
   '((<gnc:commodity-table*> table)
     (<gnc:commodity*> commodity))
   "Add a commodity to the table.")

  (gw:wrap-function
   mod
   'gnc:commodity-table-has-namespace
   '<gw:bool>
   "gnc_commodity_table_has_namespace"
   '((<gnc:commodity-table*> table)
     ((<gw:m-chars-caller-owned> gw:const) namespace))
   "Predicate to test for existence of a namespace.")

  (gw:wrap-function
   mod
   'gnc:commodity-table-get-namespaces
   '<gnc:list-of-string>
   "gnc_commodity_table_get_namespaces"
   '((<gnc:commodity-table*> table))
   "Return a list of all the namespaces in the table.")

  (gw:wrap-function
   mod
   'gnc:commodity-table-add-namespace
   '<gw:void>
   "gnc_commodity_table_add_namespace"
   '((<gnc:commodity-table*> table)
     ((<gw:m-chars-caller-owned> gw:const) namespace))
   "Add a new namespace to the commodity table")

  (gw:wrap-function
   mod
   'gnc:commodity-table-delete-namespace
   '<gw:void>
   "gnc_commodity_table_delete_namespace"
   '((<gnc:commodity-table*> table)
     ((<gw:m-chars-caller-owned> gw:const) namespace))
   "Delete a namespace from the commodity table")

  (gw:wrap-function
   mod
   'gnc:commodity-table-get-commodities
   '<gnc:list-of-commodity*>
   "gnc_commodity_table_get_commodities"
   '((<gnc:commodity-table*> table)
     ((<gw:m-chars-caller-owned> gw:const) namespace))
   "Return a list of all the namespaces in the table.")

  ;;=========

  (gw:wrap-function
   mod
   'gnc:engine-shutdown
   '<gw:void>
   "gnc_engine_shutdown"
   '()
   "Shutdown the gnucash engine.")

  ;;============
  ;; gnc_numeric

  (gw:wrap-function
   mod
   'gnc:numeric-create
   '<gnc:numeric>
   "gnc_numeric_create"
   '((<glib:gint64> num) (<glib:gint64> denom))
   "Create a new gnc_numeric object")

  (gw:wrap-function
   mod
   'gnc:numeric-zero
   '<gnc:numeric>
   "gnc_numeric_zero"
   '()
   "Create a zero-valued gnc_numeric")

  (gw:wrap-function
   mod
   'gnc:numeric-error
   '<gnc:numeric>
   "gnc_numeric_error"
   '((<gw:int> code))
   "Create an error-signaling gnc_numeric")

  (gw:wrap-function
   mod
   'gnc:numeric-check
   '<gw:int>
   "gnc_numeric_check"
   '((<gnc:numeric> val))
   "Check for error code in a gnc_numeric")

  (gw:wrap-function
   mod
   'gnc:numeric-num
   '<glib:gint64>
   "gnc_numeric_num"
   '((<gnc:numeric> arg))
   "Return the numerator of a gnc_numeric")

  (gw:wrap-function
   mod
   'gnc:numeric-denom
   '<glib:gint64>
   "gnc_numeric_denom"
   '((<gnc:numeric> arg))
   "Return the denominator of a gnc_numeric")

  (gw:wrap-function
   mod
   'gnc:numeric-zero-p
   '<gw:bool>
   "gnc_numeric_zero_p"
   '((<gnc:numeric> arg))
   "Check for zero.")

  (gw:wrap-function
   mod
   'gnc:numeric-compare
   '<gw:int>
   "gnc_numeric_compare"
   '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
   "Compare 2 gnc_numeric (1 if a>b, 0 if a==b, -1 if a<b)")

  (gw:wrap-function
   mod
   'gnc:numeric-negative-p
   '<gw:bool>
   "gnc_numeric_negative_p"
   '((<gnc:numeric> arg))
   "Check for arg < 0")

  (gw:wrap-function
   mod
   'gnc:numeric-positive-p
   '<gw:bool>
   "gnc_numeric_positive_p"
   '((<gnc:numeric> arg))
   "Check for arg > 0")

  (gw:wrap-function
   mod
   'gnc:numeric-eq
   '<gw:bool>
   "gnc_numeric_eq"
   '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
   "Check for arg1 exactly arg2")

  (gw:wrap-function
   mod
   'gnc:numeric-equal
   '<gw:bool>
   "gnc_numeric_equal"
   '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
   "Check for arg1 same number as arg2")

  (gw:wrap-function
   mod
   'gnc:numeric-same
   '<gw:bool>
   "gnc_numeric_same"
   '((<gnc:numeric> arg1)
     (<gnc:numeric> arg2)
     (<glib:gint64> denom)
     (<gw:int> how))
   "Check for arg1 same number as arg2 if converted to common denom")

  (gw:wrap-function
   mod
   'gnc:numeric-add
   '<gnc:numeric>
   "gnc_numeric_add"
   '((<gnc:numeric> arg1)
     (<gnc:numeric> arg2)
     (<glib:gint64> denom)
     (<gw:int> how))
   "Return a+b")

  (gw:wrap-function
   mod
   'gnc:numeric-sub
   '<gnc:numeric>
   "gnc_numeric_sub"
   '((<gnc:numeric> arg1)
     (<gnc:numeric> arg2)
     (<glib:gint64> denom)
     (<gw:int> how))
   "Return a-b")

  (gw:wrap-function
   mod
   'gnc:numeric-mul
   '<gnc:numeric>
   "gnc_numeric_mul"
   '((<gnc:numeric> arg1)
     (<gnc:numeric> arg2)
     (<glib:gint64> denom)
     (<gw:int> how))
   "Return a*b")

  (gw:wrap-function
   mod
   'gnc:numeric-div
   '<gnc:numeric>
   "gnc_numeric_div"
   '((<gnc:numeric> arg1)
     (<gnc:numeric> arg2)
     (<glib:gint64> denom)
     (<gw:int> how))
   "Return a/b")

  (gw:wrap-function
   mod
   'gnc:numeric-neg
   '<gnc:numeric>
   "gnc_numeric_neg"
   '((<gnc:numeric> arg))
   "Return -a")

  (gw:wrap-function
   mod
   'gnc:numeric-abs
   '<gnc:numeric>
   "gnc_numeric_abs"
   '((<gnc:numeric> arg))
   "Return |a|")

  (gw:wrap-function
   mod
   'gnc:numeric-add-fixed
   '<gnc:numeric>
   "gnc_numeric_add_fixed"
   '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
   "Return a+b under strict fixed-denom rules")

  (gw:wrap-function
   mod
   'gnc:numeric-sub-fixed
   '<gnc:numeric>
   "gnc_numeric_sub_fixed"
   '((<gnc:numeric> arg1) (<gnc:numeric> arg2))
   "Return a-b under strict fixed-denom rules")

  (gw:wrap-function
   mod
   'gnc:numeric-convert
   '<gnc:numeric>
   "gnc_numeric_convert"
   '((<gnc:numeric> arg) (<glib:gint64> denom) (<gw:int> how))
   "Convert a gnc_numeric to a new denominator")

  (gw:wrap-function
   mod
   'gnc:double-to-gnc-numeric
   '<gnc:numeric>
   "double_to_gnc_numeric"
   '((<gw:double> in) (<glib:gint64> denom) (<gw:int> how))
   "Convert <gw:double> to gnc_numeric")

  (gw:wrap-function
   mod
   'gnc:numeric-to-double
   '<gw:double>
   "gnc_numeric_to_double"
   '((<gnc:numeric> in))
   "Convert gnc_numeric to double")

  (gw:wrap-function
   mod
   'gnc:numeric-to-string
   '(<gw:m-chars-caller-owned> gw:const)
   "gnc_numeric_to_string"
   '((<gnc:numeric> arg))
   "Convert gnc_numeric to a printable string")

  (gw:wrap-function
   mod
   'gnc:run-rpc-server
   '<gw:void>
   "gnc_run_rpc_server"
   '()
   "Run the RPC Server") 

  ;; src/engine/date.h

  (gw:wrap-function
   mod
   'gnc:timepair-canonical-day-time
   '<gnc:time-pair>
   "timespecCanonicalDayTime" '((<gnc:time-pair> tp))
   "Convert a timepair on a certain day (localtime) to\
the timepair representing midday on that day")


  ;; src/engine/gnc-engine-util.h

  (gw:wrap-function
   mod
   'gnc:safe-strcmp
   '<gw:int>
   "safe_strcmp" '(((<gw:m-chars-caller-owned> gw:const) a)
		   ((<gw:m-chars-caller-owned> gw:const) b))
   "Do a string comparison - in the C case it handles NULLs as the earliest string\
but in this case it doesn't really matter as a NULL should never be passed to it
over the scheme interface.")

  )


