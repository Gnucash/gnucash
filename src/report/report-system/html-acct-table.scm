;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-acct-table.scm : generate a multi-columnar list of accounts
;; including utilities to convert to <html-table> form
;; 
;; By David Montenegro 2004.06.23 <sunrise2000@comcast.net>
;; 
;; Borrowed largely from html-table.scm by Bill Gribble <grib@gnumatic.com>
;; and html-utilities.scm by Christian Stimming <stimming@tu-harburg.de>
;; 
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;;
;; DESCRIPTION
;; 
;; The html-acct-table object is a utility object, not an html object.
;; It is used to collect and then render a table whose leftmost column(s)
;; are a list or chart of accounts.
;; 
;; You start by creating the object and initializing it with a list of
;; accounts and a few assorted parameters.  It generates a table, which
;; can be read using accessor functions, containing information which
;; makes it easy(ier) to create a great variety of html-table forms.
;; 
;;               add-accounts            add-account-balances
;;  account-list ------------> html-acct-table ----------> html-table
;;
;;    Figure Key:
;;
;;       account-list: a list of accounts as would be returned from
;;       looking up the value of a report option added with
;;       gnc:options-add-account-selection!
;;
;;       add-accounts: any method that adds the account list to the
;;       html-acct-table.  For example, this could be accomplished
;;       with gnc:make-html-acct-table/env/acct or
;;       gnc:html-acct-table-add-accounts!
;;
;;       html-acct-table: the utility object described in this file
;;
;;       add-account-balances: any function that maps the internal
;;       data of the html-acct-table object into a html-table.  For
;;       example, one such function is
;;       gnc:html-table-add-account-balances.
;;
;;       html-table: an <html-table> scheme object representing an
;;       HTML table block.  See html-table.scm.
;;
;; This utility object was written because of some shortcomings
;; inherent in how the gnc:html-build-acct-table function was
;; designed.  Ultimately, the intent is to replace
;; gnc:html-build-acct-table with an html-acct-table with the
;; appropriate thunks.  But, because this is new, I'm leaving the
;; original gnc:html-build-acct-table in place, just to be safe.
;;
;;
;; ARGUMENTS
;; 
;; For boolean arguments, #t and #f have their usual meanings.  If a
;; boolean argument is not set, a default value may be assumed.  For
;; non-boolean arguments, values may be specified.  When #f is
;; specified as the value of a non-boolean argument, it generally
;; means to omit whatever function the argument controls.  When #t is
;; specified for such an argument, it generally means to use that
;; functionality, but just accept whatever default functionality that
;; option may have.
;; 
;; The list of accounts which are to be placed in the
;; gnc:html-acct-table object can be controled with the
;; gnc:make-html-acct-table/accts, gnc:make-html-acct-table/env/accts,
;; and gnc:html-table-add-accts!  functions.  But you should only use
;; one of these methods to add accounts.
;; 
;; The gnc:html-acct-table parameters should be set BEFORE adding the
;; account list.  They can be set with gnc:make-html-acct-table/env
;; or gnc:make-html-acct-table/accts/env and fetched with
;; gnc:html-acct-table-env; accept the following parameters:
;; 
;;     display-tree-depth: integer 'unlimited ['all] #f
;; 
;;         the number of levels of accounts to display
;;         'unlimited, 'all, and #f impose no depth limit.
;;         the default is 'all.  [CAS: ISTM, the default is actually #f,
;;         and this case bombs at (< logi-depth depth-limit) anytime the
;;         limit behavior is not 'flatten.  BUG?   Also, setting this
;;         parameter to a large integer value has the strange side-effect
;;         of pushing the balances column far right, even when the account
;;         tree is relatively shallow.]
;; 
;;     depth-limit-behavior: ['summarize] 'flatten 'truncate
;; 
;;         when the display tree reaches its depth limit, this option
;;         tells gnc:html-acct-table what to do.  'summarize tells it
;;         to omit accounts below the depth limit and summarize their
;;         contents as belonging to their parent account at the depth
;;         limit. 'flatten tells it to display every selected
;;         subaccount, all the way down the tree, but to position
;;         them, in the chart, at the depth limit. the default value
;;         is 'summarize
;; 
;;     initial-indent: integer
;; 
;;         the number of table cells to indent the first level of
;;         accounts displayed. this is merely a convenience.  the
;;         default initial-indent is 0.
;; 
;;     account-less-p: binary_predicate #t #f
;; 
;;         used for sorting accounts, below each parent account, into
;;         the order in which they will be displayed.  the function
;;         must take two Account arguments and represent a total
;;         ordering on Account-space.  #t means to use the default
;;         sorting function.  #f means to preform no sorting.  the
;;         default sorting function is gnc:account-code-less-p.
;; 
;;     start-date: timepair
;; 
;;         the starting date of the reporting period over which to
;;         report balances for this account.  if start-date is #f,
;;         will be no limit on how early a counted transaction may
;;         ocurr.
;; 
;;     end-date: timepair
;; 
;;         the ending date of the reporting period over which to
;;         report balances for this account.  if end-date is #f, there
;;         will be no limit on how late a counted transaction may
;;         ocurr. note: i do not know if GnuCash, right now, supports
;;         transactions in the future. so be prepared for the
;;         possibility that this may match transactions which haven't
;;         ocurred, yet. [CAS: I don't think end-date of #f works.
;;         It bombs.]
;; 
;;     report-commodity: commodity
;; 
;;         the commodity into which to convert any balances containing
;;         foreign currencies.  the balance will be converted using
;;         the exchange function exchange-fn. the default is the
;;         currency returned by (gnc-default-report-currency).  [CAS:
;;         what if I don't want the report to have a
;;         "report-commodity"?  Say e.g. I want to show each account
;;         balance in its native commodity?  I can see the benefit of
;;         individual reports that have a report-commodity using
;;         gnc-default-report-currency to set the default value for a
;;         report-commodity option.  But, with the default sucked in
;;         here, in what is supposed to be a more general api, means
;;         reports can't specify NO report-commodity. ]
;;
;; CAS: Hypothetical design modification: Instead of specifying a
;; single report commodity and exchange-fn for the whole acct-table,
;; what if we were allowed to specify a *price-source* and an
;; *optional* report-commodity.  Then, if the no report commodity is
;; specified, then we can generate the exchange-fn on a per-account
;; basis, using gnc:case-exchange-fn and the account's native
;; commodity and the given price-source.  Otherwise (i.e. if there IS
;; a report-commodity specified), we can use *that* commodity for each
;; account's exchange-fn.
;;
;;     exchange-fn: commodity_exchange_function
;; 
;;         the commodity exchange function (you know, that weighted
;;         average, most recent, nearest in time fun stuff) used to
;;         convert balances which are not exclusively in the report
;;         commodity into the report commodity.  [CAS: Right now,
;;         exchange-fn is not optional.  If your accounts have
;;         different commodities and you don't specify a valid
;;         exchange function then simply adding the accounts to the
;;         html-acct-table object will crash, even if you never want
;;         to display any values at all.  This is bad.  UPDATE: As a
;;         short-term fix, I've made this parameter optional.  If no
;;         exchange-fn is given, you can at least add the accounts to
;;         the html-acct-table object without crashing.  Just don't
;;         count on meaningful report-comm-{account|recursive}-bal
;;         values (they'll also be #f).]
;;
;;     column-header: html-table-header-cell #f #t
;; 
;;          the table column header cell (TH tag) with which to head
;;          the columns containing the account tree.  if supplied, the
;;          header cell may contain style information.  if #f, no
;;          column header cell will be used.  if #t, a default header
;;          cell (reading "Account") will be used.  the colspan of any
;;          header cell will be automatically set appropriately. this
;;          is for convenience only; gnc:html-acct-table does not use
;;          this data.
;; 
;;     account-label-mode: 'name 'anchor
;; 
;;          tells whether to render account labels as hyperlinks or
;;          text.  stylesheets, really, should be able to remove
;;          link markup.
;; 
;;     parent-account-subtotal-mode: #t #f 'canonically-tabbed
;; 
;;          indicates whether or not to add a line, recursively
;;          subtotalling an account and its descendents, for any
;;          account with children (non-leaf account).  if #t or
;;          #canonically-tabbed, a subtotal row will be created for
;;          each non-leaf account.  if #f, no non-leaf account
;;          subtotal rows will be created.  if 'canonically-tabbed,
;;          account total entry labels will be placed at the position
;;          specified by accounting texts (indented one column from
;;          the accounts being totalled, two columns from where
;;          gnc:html-acct-table would otherwise place them). the
;;          default is #f.
;; 
;;     zero-balance-mode: 'show-leaf-acct 'omit-leaf-acct
;; 
;;          indicates what to do with accounts with zero balance. if
;;          'omit-leaf-acct, no account row will be generated for any
;;          account having a balance of zero. otherwise, a row will be
;;          generated for the account.
;; 
;;     balance-mode: 'pre-adjusting 'pre-closing 'post-closing
;;
;;          indicates whether or not to ignore adjusting/closing
;;          entries when computing account balances. 'pre-closing
;;          ignores only closing entries. 'pre-adjusting also ignores
;;          adjusting entries. 'post-closing counts all entries.
;; 
;;     adjusting-pattern: alist of 'str 'cased 'regexp
;; 
;;          a pattern alist, as accepted by
;;          gnc:account-get-trans-type-balance-interval, matching
;;          adjusting transactions to be ignored when balance-mode is
;;          'pre-adjusting.
;; 
;;     closing-pattern: alist of 'str 'cased 'regexp
;; 
;;          a pattern alist, as accepted by
;;          gnc:account-get-trans-type-balance-interval, matching
;;          closing transactions to be ignored when balance-mode is
;;          'pre-closing.
;; 
;;     account-type: unimplemented
;;     account-class: unimplemented
;;     row-thunk: unimplemented (for gnc:html-acct-table-render)
;;     row-list: unimplemented (list of all the rows ever added)
;; 
;; The html-acct-table object lets you generate, store, and access the
;; following parameters:
;; 
;;     account: Account
;; 
;;         the account in the current row
;; 
;;     account-parent: Account #f
;; 
;;         the parent account of the current account, if one exists.
;;         #f if the current account has no parent.
;; 
;;     account-guid: guid
;; 
;;         the guid of the account in the current row, as returned by
;;         gncAccountGetGUID.
;; 
;;     account-desc: string?
;; 
;;         the account description of the account in the current row,
;;         as returned by xaccAccountGetDescription.
;; 
;;     account-notes: string?
;; 
;;         the account notes of the account in the current row, as
;;         returned by xaccAccountGetNotes.
;; 
;;     account-path: string
;; 
;;         the full name of the account in the current row. i.e., if
;;         the name of the account is "Assets:Current Assets:Cash",
;;         the value will be "Assets:Current Assets:Cash".
;; 
;;     account-name: string
;; 
;;         the "basename" of the account in the current row. i.e., if
;;         the name of the account is "Assets:Current Assets:Cash",
;;         the value will be "Cash".
;; 
;;     account-code: string
;; 
;;         the account of the account in the current row, as returned
;;         by xaccAccountGetCode.
;; 
;;     account-anchor: text(maybe?)
;; 
;;         a link to the account in the current row
;; 
;;     account-label: string
;; 
;;         the text used to label the account in the current row.  if
;;         account-label-mode is 'name, this consists of account-name
;;         prepended, if row-type is 'subtotal-row, by "Total ".  if
;;         account-label-mode is 'anchor, this consists of
;;         account-anchor prepended, if row-type is 'subtotal-row, by
;;         "Total ".
;; 
;;     account-depth: integer
;; 
;;         the depth at which the account in the current row resides
;;         in the account tree. note that this may differ from
;;         display-depth when depth-limit-behavior is 'flatten.
;;         unlike in gnc:html-build-acct-table, the first level of
;;         accounts is level 0.
;; 
;;     logical-depth: integer
;; 
;;         the depth at which the account in the current row resides
;;         in the effective account tree.  this is the depth the
;;         account tree when ignoring unselected parent accounts.
;;         note that this may differ from account-depth when a
;;         selected account has an unselected ancestor.
;; 
;;     display-depth: integer
;; 
;;         the depth at which the account in the current row resides
;;         in the display tree. note that this may differ from
;;         account-depth when depth-limit-behavior is 'flatten.
;;         unlike in gnc:html-build-acct-table, the first level of
;;         accounts is level 0. this means that display-depth is also
;;         the number of empty cells which should preceed the account
;;         name in the gnc:html-table being generated.
;; 
;;     indented-depth: integer
;; 
;;         the depth at which the account in the current row resides
;;         in the indented display tree. also account-depth plus
;;         indent.  CAS: I think *display-depth* plus indent would
;;         make more sense.  Then it's like an absolute column index.
;; 
;;     logical-cols: integer
;; 
;;         the number of columns in which account labels were placed.
;; 
;;     label-cols: integer
;; 
;;         the number of columns in the group of account columns to
;;         which a row was assigned.  also one more than the maximum
;;         column depth at which rows were positioned in the
;;         table. this value may be different from logical-cols when
;;         parent-account-subtotal-mode is 'canonically-tabbed.
;; 
;;     account-cols: integer
;; 
;;         the number of columns in the group of account columns.  if
;;         display-tree-depth is #f, this is the value of label-cols
;;         plus any indent.  if display-tree-depth is set, this is the
;;         value of display-tree-depth, plus indent plus zero, if
;;         parent-account-subotal-mode is not 'canonically-tabbed, or,
;;         if parent-account-subtotal-mode is 'canonically-tabbed,
;;         plus one.  dont you just love english?
;; 
;;     account-colspan: integer
;; 
;;         the number of table columns which the account label of the
;;         account in the current row should span in the
;;         gnc:html-table being generated.
;; 
;;     account-children: list of Accounts
;; 
;;         a list of all children of the account in the current row.
;; 
;;     account-bal: commodity-collector
;; 
;;         the balance of the account in the current row, exclusive of
;;         any balances in any subaccounts.  this is for convenience.
;; 
;;     recursive-bal: commodity-collector
;; 
;;         the balance of the account in the current row, recursively
;;         including all balances in any *selected* subaccounts.  this
;;         is for convenience.
;; 
;; CAS: I think these next two are wrong because they are really of
;; type gnc:monetary, not commodity-collectors.
;;
;;     report-comm-account-bal: commodity-collector
;; 
;;         the balance of the account in the current row, exclusive of
;;         any balances in any subaccounts, converted to
;;         report-commodity using exchange-fn.  this is for
;;         convenience.
;; 
;;     report-comm-recursive-bal: commodity-collector
;; 
;;         the balance of the account in the current row, recursively
;;         including all balances in any *selected* subaccounts,
;;         converted to report-commodity using exchange-fn.  this is
;;         for convenience.
;; 
;;     account-commodity: commodity
;; 
;;         returns the default commodity of the account in the current
;;         row, as returned by xaccAccountGetCommodity. 
;; 
;;     account-type: account_type
;; 
;;         returns the type of the account in the current row
;; 
;;     account-type-string: string
;; 
;;         returns the type of the account in the current row as a
;;         string
;; 
;;     row-type: 'account-row 'subtotal-row 
;; 
;;         indicates the nature of the current row.  'account-row
;;         indicates that the current row represents an account
;;         balance.  'subtotal-row indicates that it represents a
;;         subtotal.
;; 
;; 
;; DIFFERENCES FROM PARAMETERS USED BY gnc:html-build-acct-table
;; 
;; The show-subaccounts? option of gnc:html-build-acct-table, which
;; used to select an accounts recursively like the "-R" option to ls,
;; has been removed.  I find it both confusing, as a user, and
;; obfuscating, as a programmer.  Any accounts which are to be
;; included in the report may be selected in the Accounts options
;; widget.  While, when selecting whole subtrees of accounts, this may
;; be tedious, this really is a GUI problem.  The ideal solution would
;; be to give the Account selection widget a "recursively select"
;; option which selects (i.e., hilights) both the account selected and
;; all its subaccounts.  Note that, as a worst-case workaround, the
;; user could always use the spacebar and arrow keys to select entire
;; subtrees rather rapidly.  It order to make this shortcoming as
;; benign as possible, reports are advised to make the default account
;; selection that which is closest to what the report user is likely
;; to select.  It is my hope that a recursive account selection widget
;; will soon be implemented.
;; 
;; The group-types? option of gnc:html-build-acct-table, which
;; would display accounts by account type and supply header and
;; total lines for each type (see source), has been removed.
;; It is easy enough to duplicate this functionality, report-side,
;; using the new gnc:html-acct-table object.
;;
;; The start-percent and delta-percent options of
;; gnc:html-build-acct-table, which told the function to
;; gnc:report-percent-done start-percent to
;; (start-percent+start-delta) percent of the progress bar, has been
;; removed.  Most of the report building is done while reading the
;; gnc:html-acct-table object, anyway, so this is not a great loss.
;; This functionality should, however, be included as the amount of
;; work required to build an gnc:html-acct-table object is
;; non-trivial.  Being non-critical as it is, this is left as a future
;; project.  Since much of the code for this has already been written
;; (in gnc:html-build-acct-table), when the time comes, this
;; functionality should not be difficult to add.
;; 
;; 
;; INTERNALS
;; 
;; Internally, html-acct-table uses an html-table object to store
;; data.  Since the html-acct-table object is arguably a more general
;; class than html-table, one might think that the html-table object
;; should be written to use an html-acct-table for data storage,
;; manipulation, and access.  The html-table class, as it happens, was
;; written first, so the decision was made to use it rather than
;; redesign the horse around the carriage.
;; 
;; It may also be possible to have made html-acct-table a markup/style
;; sheet pair.  To do this, the html-acct-table (which would
;; essentially be a markup object) would have to store thunks and call
;; them when rendering its contents to its parent html-doc.  This
;; means that report code would be called during stylization, rather
;; than while building the report.  Making html-acct-table a utility
;; object means that one can use it in a report generator in a
;; programmatic manner, keeping clear the separation between report
;; generation and stylization.
;; 
;; The first cell in each row of the html-table consist of an a-list
;; of row-parameters.  These parameters are described in PARAMETERS
;; above.  Any remaining cells in the row represent data set by the
;; user.  This class simply maps its contents to the html-table.
;; 

;; this is to work around a bug in the HTML export sytmem
;; which causes COLSPAN= attributes not to be exported (!!)
(define gnc:colspans-are-working-right #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-acct-table> class
;;  utility class for generating account tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-acct-table>
  (make-record-type "<html-acct-table>"
		    '(matrix ;; an html-table
		      env    ;; an alist
		      )))

(define gnc:html-acct-table? 
  (record-predicate <html-acct-table>))

(define gnc:_make-html-acct-table_
  (record-constructor <html-acct-table>))

(define (gnc:make-html-acct-table)
  (gnc:_make-html-acct-table_
   (gnc:make-html-table) ;; matrix
   #f                    ;; env
   ))

(define (gnc:make-html-acct-table/env env)
  (let ((acct-table (gnc:make-html-acct-table)))
    (gnc:html-acct-table-set-env! acct-table env)
    acct-table))

(define (gnc:make-html-acct-table/env/accts env accts)
  (let ((acct-table (gnc:make-html-acct-table)))
    ;; the env must be set *before* the accounts...  because the env
    ;; parameter end-date is required by
    ;; gnc:html-acct-table-add-accounts!.
    (gnc:_html-acct-table-set-env!_ acct-table env)
    (gnc:html-acct-table-add-accounts! acct-table accts)
    acct-table))

(define gnc:_html-acct-table-matrix_
  (record-accessor <html-acct-table> 'matrix))

(define gnc:_html-acct-table-set-matrix!_
  (record-modifier <html-acct-table> 'matrix))

(define gnc:_html-acct-table-env_
  (record-accessor <html-acct-table> 'env))

(define gnc:_html-acct-table-set-env!_
  (record-modifier <html-acct-table> 'env))

;; some useful predicates to export
(define (gnc:account-code-less-p a b)
  (string<? (xaccAccountGetCode a)
	    (xaccAccountGetCode b)))
(define (gnc:account-name-less-p a b)
  (string<? (xaccAccountGetName a)
	    (xaccAccountGetName b)))
(define (gnc:account-path-less-p a b)
  (string<? (gnc-account-get-full-name a)
	    (gnc-account-get-full-name b)))


(define (gnc:html-acct-table-add-accounts! acct-table accounts)
  ;; 
  ;; This is where most of the html-acct-table functionality ends up....
  ;; 
  ;; This function traverses the (current) account tree, adding
  ;; information about the selected accounts to acct-table.
  ;; 
  
  ;; helper for fetching values from the key/val environment alist
  (define (get-val alist key)
    (let ((lst (assoc-ref alist key)))
      (if lst (car lst) lst)))



  ;; helper to plop <env> in the next available env cell
  (define (add-row env)
    (let ((html-table (gnc:_html-acct-table-matrix_ acct-table)))
      (gnc:html-table-set-cell!
       html-table
       (gnc:html-table-num-rows html-table)
       0
       env)
      )
    )
  
  (let* ((env (gnc:_html-acct-table-env_ acct-table))
	 ;; establish all input parameters and their defaults 
	 (depth-limit (let ((lim (get-val env 'display-tree-depth)))
			(if (or (equal? lim 'unlimited)
				(equal? lim 'all))
			    #f ;; BUG?  other code expects integer here
			    lim)))
	 (limit-behavior (or (get-val env 'depth-limit-behavior) 'summarize))
	 (indent (or (get-val env 'initial-indent) 0))
	 (less-p (let ((pred (get-val env 'account-less-p)))
		   (if (equal? pred #t) gnc:account-code-less-p pred)))
	 (start-date (get-val env 'start-date))
	 (end-date (or (get-val env 'end-date)
		       (cons 'absolute (cons (current-time) 0))))
	 (report-commodity (or (get-val env 'report-commodity)
			       (gnc-default-report-currency)))
         ;; BUG: other code expects a real function here, maybe
         ;; someone was thinking price-source?
	 (exchange-fn (or (get-val env 'exchange-fn)
                          #f))
         ;;'weighted-average))
	 (column-header (let ((cell (get-val env 'column-header)))
			  (if (equal? cell #t)
			      (gnc:make-html-table-cell "Account name")
			      cell)))
	 (subtotal-mode (get-val env 'parent-account-subtotal-mode))
	 (zero-mode (let ((mode (get-val env 'zero-balance-mode)))
		      (or (if (equal? mode #t) 'show-leaf-acct mode)
			  'show-leaf-acct)
		      ))
	 (label-mode (or (get-val env 'account-label-mode) 'anchor))
	 (balance-mode (or (get-val env 'balance-mode) 'post-closing))
	 (closing-pattern (or (get-val env 'closing-pattern)
			      (list
			       (list 'str (N_ "Closing Entries"))
			       (list 'cased #f)
			       (list 'regexp #f)
			       )
			      ))
	 (adjusting-pattern (or (get-val env 'adjusting-pattern)
				(list
				 (list 'str (N_ "Adjusting Entries"))
				 (list 'cased #f)
				 (list 'regexp #f)
				 )
				))
	 ;; local variables
	 (toplvl-accts
	  (gnc-account-get-children-sorted (gnc-get-current-root-account)))
	 (acct-depth-reached 0)
	 (logi-depth-reached (if depth-limit (- depth-limit 1) 0))
	 (disp-depth-reached 0)
	 )

    ;; the following function was adapted from html-utilities.scm
    ;; 
    ;;
    ;; there's got to be a prettier way to do this. maybe even make two
    ;; of these. The balance-mode is only used by trial-balance.scm. so 
    ;; make two versions of this animal, one that cares about balance-mode 
    ;; one that doesn't. then check for a balance-mode !'post-closing and
    ;; call the right one. later.
    (define (get-balance-nosub-mode account start-date end-date)
      (let* ((post-closing-bal
	      (if start-date
		  (gnc:account-get-comm-balance-interval
		   account start-date end-date #f)
		  (gnc:account-get-comm-balance-at-date
		   account end-date #f)))
	     (closing (lambda(a)
			(gnc:account-get-trans-type-balance-interval
			 (list account) closing-pattern
			 start-date end-date)
			)
		      )
	     (adjusting (lambda(a)
			  (gnc:account-get-trans-type-balance-interval
			   (list account) adjusting-pattern
			   start-date end-date)
			  )
			)
	     )
	;; what the heck is this? how about (case balance-mode blah)...
	(or (and (equal? balance-mode 'post-closing) post-closing-bal)
	    (and (equal? balance-mode 'pre-closing)
		 (let* ((closing-amt (closing account))
			)
		   (post-closing-bal 'minusmerge closing-amt #f)
		   post-closing-bal)
		 )
	    (and (equal? balance-mode 'pre-adjusting)
		 (let* ((closing-amt (closing account))
			(adjusting-amt (adjusting account))
			)
		   (post-closing-bal 'minusmerge closing-amt #f)
		   (post-closing-bal 'minusmerge adjusting-amt #f)
		   post-closing-bal)
		 )
	    ;; error if we get here.
	    )
	)
      )

    ;; helper to calculate the balances for all required accounts
    (define (calculate-balances accts start-date end-date)
      (define (calculate-balances-helper accts start-date end-date acct-balances)
        (if (not (null? accts))
            (begin
                ;; using the existing function that cares about balance-mode
                ;; maybe this should get replaces at some point.
                (hash-set! acct-balances (gncAccountGetGUID (car accts))
                    (get-balance-nosub-mode (car accts) start-date end-date))
                (calculate-balances-helper (cdr accts) start-date end-date acct-balances)
            )
            acct-balances)
        )
        
      (calculate-balances-helper accts start-date end-date
                                 (make-hash-table 23))                                 
      )


    (define (traverse-accounts! accts acct-depth logi-depth new-balances)
      
      (define (use-acct? acct)
	;; BUG?  when depth-limit is not integer but boolean?
	(and (or (equal? limit-behavior 'flatten) (< logi-depth depth-limit))
	     (member acct accounts)
	     )
	)
      
      ;; helper function to return a cached balance from a list of 
      ;; ( acct . balance ) cells
      (define (get-balance acct-balances acct)
	(let ((this-collector (gnc:make-commodity-collector)))
	  (gnc-commodity-collector-merge 
	   this-collector 
	   (or (hash-ref acct-balances (gncAccountGetGUID acct))
	       ;; return a zero commodity collector
	       (gnc:make-commodity-collector)
	       )
	   )
	  this-collector
	  )
	)

      
      ;; helper function that returns a cached balance  from a list of
      ;; ( acct . balance ) cells for the given account *and* its 
      ;; sub-accounts.
      (define (get-balance-sub acct-balances account)
	;; its important to make a *new* collector for this, otherwise we're dealing with 
	;; pointers to the current collectors in our acct-balances hash and that's a 
	;; problem -- the balances get changed.
	(let ((this-collector (gnc:make-commodity-collector)))
	  ;; get the balance of the parent account and stick it on the collector
	  ;; that nice shiny *NEW* collector!!
	  (gnc-commodity-collector-merge this-collector (get-balance acct-balances account))
	  (for-each
	   (lambda (x) (if x (gnc-commodity-collector-merge this-collector x)))
	   (gnc:account-map-descendants
	    (lambda (a)
	      (get-balance acct-balances a ))
	    account))
	  this-collector))
      
      
      (let ((disp-depth
	     (if (integer? depth-limit)
		 (min (- depth-limit 1) logi-depth)
		 logi-depth))
	    )
	
	(for-each
	 (lambda (acct)
	   (let* ((subaccts (gnc-account-get-children-sorted acct))
		  ;; assign output parameters
		  (account acct)
		  (account-name (xaccAccountGetName acct))
		  (account-code (xaccAccountGetCode acct))
		  (account-path (gnc-account-get-full-name acct))
		  (account-anchor (gnc:html-account-anchor acct))
		  (account-parent (gnc-account-get-parent acct))
		  (account-children subaccts)
		  (account-depth acct-depth)
		  (logical-depth logi-depth)
		  (account-commodity (xaccAccountGetCommodity acct))
		  (account-type (xaccAccountGetType acct))
		  ;; N.B.: xaccAccountGetTypeStr really should be
		  ;; called gnc:account-type-get-string
		  (account-type-string (xaccAccountGetTypeStr
					(xaccAccountGetType acct)))
		  (account-guid (gncAccountGetGUID acct))
		  (account-description (xaccAccountGetDescription acct))
		  (account-notes (xaccAccountGetNotes acct))
		  ;; These next two are commodity-collectors.
		  (account-bal (get-balance
				new-balances acct))
		  (recursive-bal (get-balance-sub
				  new-balances acct))
		  ;; These next two are of type <gnc:monetary>, right?
		  (report-comm-account-bal
		   (gnc:sum-collector-commodity
		    account-bal report-commodity exchange-fn))
		  (report-comm-recursive-bal
		   (gnc:sum-collector-commodity
		    recursive-bal report-commodity exchange-fn))
		  (grp-env
		   (append env
			   (list
			    (list 'initial-indent indent)
			    (list 'account account)
			    (list 'account-name account-name)
			    (list 'account-code account-code)
			    (list 'account-type account-type)
			    (list 'account-type-string account-type-string)
			    (list 'account-guid account-guid)
			    (list 'account-description account-description)
			    (list 'account-notes account-notes)
			    (list 'account-path account-path)
			    (list 'account-parent account-parent)
			    (list 'account-children account-children)
			    (list 'account-depth account-depth)
			    (list 'logical-depth logical-depth)
			    (list 'account-commodity account-commodity)
			    (list 'account-anchor account-anchor)
			    (list 'account-bal account-bal)
			    (list 'recursive-bal recursive-bal)
			    (list 'report-comm-account-bal
				  report-comm-account-bal)
			    (list 'report-comm-recursive-bal
				  report-comm-recursive-bal)
			    (list 'report-commodity report-commodity)
			    (list 'exchange-fn exchange-fn)
			    )))
		  (row-env #f)
		  (label (or (and (equal? label-mode 'anchor)
				  account-anchor)
			     (and (equal? label-mode 'name)
				  (gnc:make-html-text account-name))
			     ))
		  )

	     (set! acct-depth-reached (max acct-depth-reached acct-depth))
	     (set! logi-depth-reached (max logi-depth-reached logi-depth))
	     (set! disp-depth-reached (max disp-depth-reached disp-depth))
	     (or (not (use-acct? acct))
		 ;; ok, so we'll consider parent accounts with zero
		 ;; recursive-bal to be zero balance leaf accounts
		 (and (gnc-commodity-collector-allzero? recursive-bal)
		      (equal? zero-mode 'omit-leaf-acct))
		 (begin
		   (set! row-env
			 (append grp-env
				 (list
				  (list 'account-label label)
				  (list 'row-type 'account-row)
				  (list 'display-depth disp-depth)
				  (list 'indented-depth
					(+ disp-depth indent))
				  )
				 ))
		   (add-row row-env)
		   )
		 )
	     ;; Recurse:
	     ;; Dive into an account even if it isnt selected!
	     ;; why? because some subaccts may be selected.
	     (traverse-accounts! subaccts
				 (+ acct-depth 1)
				 (if (use-acct? acct)
				     (+ logi-depth 1)
				     logi-depth)
				 new-balances)

	     ;; after the return from recursion: subtotals
	     (or (not (use-acct? acct))
		 (not subtotal-mode)
		 ;; ditto that remark concerning zero recursive-bal...
		 (and (gnc-commodity-collector-allzero? recursive-bal)
		      (equal? zero-mode 'omit-leaf-acct))
		 ;; ignore use-acct for subtotals...?
		 ;; (not (use-acct? acct))
		 (null? subaccts)
		 (let* ((lbl-txt (gnc:make-html-text (_ "Total") " ")))
		   (apply gnc:html-text-append! lbl-txt
			  (gnc:html-text-body label))
		   (if (equal? subtotal-mode 'canonically-tabbed)
		       (set! disp-depth (+ disp-depth 1))
		       (set! disp-depth-reached
			     (max disp-depth-reached disp-depth))
		       )
		   (set! row-env
			 (append grp-env
				 (list
				  (list 'account-label lbl-txt)
				  (list 'row-type 'subtotal-row)
				  (list 'display-depth disp-depth)
				  (list 'indented-depth
					(+ disp-depth indent))
				  )
				 ))
		   (add-row row-env)
		   )
		 )
	     )) ;; end of (lambda (acct) ...)
	 ;; lambda is applied to each item in the (sorted) account list
	 (if less-p
	     (sort accts less-p)
	     accts)
	 ) ;; end of for-each
	)
      ) ;; end of definition of traverse-accounts!

    ;; do it
    (traverse-accounts! toplvl-accts 0 0 (calculate-balances accounts start-date end-date))
    
    ;; set the column-header colspan
    (if gnc:colspans-are-working-right
	(if (gnc:html-table-cell? column-header)
	    (gnc:html-table-cell-set-colspan! column-header
					      (+ disp-depth-reached 1 indent))
	    )
	)
    
    ;; now set the account-colspan entries
    ;; he he... (let ((x 0)) (while (< x 5) (display x) (set! x (+ x 1))))
    ;; now I know how to loop in scheme... yay!
    (let ((row 0)
	  (rows (gnc:html-acct-table-num-rows acct-table)))
      (while (< row rows)
	     (let* ((orig-env
		     (gnc:html-acct-table-get-row-env acct-table row))
		    (display-depth (get-val orig-env 'display-depth))
		    (depth-limit (get-val orig-env 'display-tree-depth))
		    (indent (get-val orig-env 'initial-indent))
		    (indented-depth (get-val orig-env 'indented-depth))
		    (subtotal-mode
		     (get-val orig-env 'parent-account-subtotal-mode))
		    (label-cols (+ disp-depth-reached 1))
		    (logical-cols (if depth-limit
				      (min
				       (+ logi-depth-reached 1)
                                       ;; BUG?  when depth-limit is not integer?
				       depth-limit)
				      (+ logi-depth-reached 1)))
		    (colspan (- label-cols display-depth))
		    ;; these parameters *should* always, by now, be set...
		    (new-env
		     (append
		      orig-env
		      (list
		       (list 'account-colspan colspan)
		       (list 'label-cols label-cols)
		       (list 'logical-cols logical-cols)
		       (list 'account-cols
			     (+ indent
				(max label-cols
				     (if depth-limit depth-limit 0)
				     )
				)
			     )
		       )
		      ))
		    )
	       (gnc:html-acct-table-set-row-env! acct-table row new-env)
	       (set! row (+ row 1))))
      )
    
    ;; done
    
    )
  )

(define (gnc:html-acct-table-num-rows acct-table)
  (gnc:html-table-num-rows (gnc:_html-acct-table-matrix_ acct-table)))

(define (gnc:html-acct-table-num-cols acct-table)
  (- (gnc:html-table-num-columns (gnc:_html-acct-table-matrix_ acct-table)) 1))

(define (gnc:html-acct-table-get-cell acct-table row col)
  ;; we'll only ever store one object in an html-table-cell
  ;; returns the first object stored in that cell
  (let* ((cell (gnc:html-table-get-cell
		(gnc:_html-acct-table-matrix_ acct-table)
		row (+ col 1))))
    (and cell (car (gnc:html-table-cell-data cell)))
    )
  )

(define (gnc:html-acct-table-set-cell! acct-table row col obj)
  (gnc:html-table-set-cell!
   (gnc:_html-acct-table-matrix_ acct-table)
   row (+ col 1)
   obj))

(define (gnc:html-acct-table-get-row-env acct-table row)
  (gnc:html-acct-table-get-cell acct-table row -1)
  )

(define (gnc:html-acct-table-set-row-env! acct-table row env)
  (gnc:html-acct-table-set-cell! acct-table row -1 env))

(define (gnc:html-acct-table-append-row acct-table objects)
  (gnc:html-table-append-row!
   (gnc:_html-acct-table-matrix_ acct-table)
   (map
    (lambda (x) (gnc:make-html-table-cell (list x)))
    objects)))

(define (gnc:html-acct-table-prepend-row! acct-table newrow)
  (gnc:html-table-prepend-row!
   (gnc:_html-acct-table-matrix_ acct-table)
   (map
    (lambda (x) (gnc:make-html-table-cell (list x)))
    objects)))

(define (gnc:html-acct-table-append-col acct-table objects)
  (gnc:html-table-append-col!
   (gnc:_html-acct-table-matrix_ acct-table)
   (map
    (lambda (x) (gnc:make-html-table-cell (list x)))
    objects)))

(define (gnc:html-acct-table-prepend-col! acct-table newrow)
  (gnc:html-table-prepend-col!
   (gnc:_html-acct-table-matrix_ acct-table)
   (map
    (lambda (x) (gnc:make-html-table-cell (list x)))
    objects)))

(define (gnc:html-acct-table-remove-last-row! acct-table)
  (gnc:html-table-remove-last-row! (gnc:_html-acct-table-matrix_ acct-table)))

;; don't think we need this.
;;(define (gnc:identity i) i)

(define (gnc:html-acct-table-render acct-table doc)
  ;; this will be used if we ever decide to let the utility object
  ;; render a document by calling thunks registered in the row-envs...
  ;; but, for now, this (optional) feature is left unimplemented...
  #f
  )

;; 
;; Here are some standard functions to help process gnc:html-acct-tables.
;; 

(define (gnc:html-make-nbsps n)
  (if (> n 0)
      (string-append "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" (gnc:html-make-nbsps (- n 1)))
      ""))

;; Stylesheets define the following cell styles which these functions
;; use: "text-cell" "total-label-cell" "number-cell"
;; "total-number-cell".  Row styles include "normal-row",
;; "alternate-row", "primary-subheading", "secondary-subheading", and
;; "grand-total".
;; There really should also be a "first-number-cell"
;; and "last-number-cell" to put currency symbols and underlines,
;; respectively, on the numbers.

;; Note: arguably, this procedure belongs in html-table.scm instead of here.
(define (gnc:html-table-add-labeled-amount-line!
         ;; function to add a label and/or amount (which we'll call a "line")
         ;; to the end of a gnc:html-table. all depths are zero-indexed.
	 html-table
         table-width       ;; if #f defaults to (amount-depth + amount-colspan)
         row-markup        ;; optional
         total-rule?       ;; Place an <hr> in the cell previous to label?
	 label             ;; the actual label text
         label-depth       ;; defaults to zero
         label-colspan     ;; defaults to one
         label-markup      ;; optional
	 amount            ;; a <gnc:monetary> or #f
         amount-depth      ;; defaults to (label-depth + label-colspan)
         amount-colspan    ;; defaults to one
         amount-markup)    ;; optional
  (let* ((lbl-depth (or label-depth 0))
	 (lbl-colspan (if gnc:colspans-are-working-right
			  (or label-colspan 1)
			  1))
	 (amt-depth (or amount-depth (+ lbl-depth lbl-colspan)))
	 (amt-colspan (if gnc:colspans-are-working-right
			  (or amount-colspan 1)
			  1))
	 (tbl-width (or table-width (+ amt-depth amt-colspan)))
	 (row
	  (append
	   (list
	    (if label-markup                      ;; the actual label
		(gnc:make-html-table-cell/size/markup
		 1 1 label-markup (gnc:make-html-text (gnc:html-make-nbsps lbl-depth)) label)
		(gnc:make-html-table-cell/size
		 1 1 (gnc:make-html-text (gnc:html-make-nbsps lbl-depth)) label))
	    )
	   (gnc:html-make-empty-cells             ;; padding after label
            (+ (- amt-depth (/ tbl-width 2))
               (if total-rule? -1 0)
               )
            )
	   (if total-rule?                        ;; include <hr>?
	       (list (gnc:make-html-table-cell
		      (gnc:make-html-text (gnc:html-markup-hr))))
	       (list)
	       )
	   (list
	    (if amount-markup                     ;; the amount
		(gnc:make-html-table-cell/size/markup
		 1 amt-colspan amount-markup amount)
		(gnc:make-html-table-cell/size
		 1 amt-colspan amount))
	    )
	   (gnc:html-make-empty-cells             ;; padding out to full width
	    (- tbl-width (+ amt-depth amt-colspan)))
	   )
          ) ;; end of row
	 )
    (if row-markup
	(gnc:html-table-append-row/markup! html-table row-markup row)
	(gnc:html-table-append-row! html-table row))))

(define (gnc-commodity-table amount report-commodity exchange-fn)
  ;; this creates a small two-column table listing each commodity
  ;; balance and its respective report balance.  note that this
  ;; shows report-commodity amounts twice: first as a commodity
  ;; and second in the report commodity.  though this may arguably
  ;; be a bit redundant, i beleive that it makes the report more
  ;; readable.
  (let* ((table (gnc:make-html-table))
	 )
    (gnc-commodity-collector-map
     amount
     (lambda (curr val)
       (let ((bal (gnc:make-gnc-monetary curr val)))
	 (gnc:html-table-append-row!
	  table
	  (list
	   ;; add the account balance in the respective commodity
	   (gnc:make-html-table-cell/markup
	    "number-cell" bal)
	   ;; add the account balance in the report commodity
	   (gnc:make-html-table-cell/markup
	    "number-cell" (exchange-fn bal report-commodity))
	   )
	  )
	 )))
    table)
  )

;; 
;; This function adds all the lines from a gnc:html-acct-table to a
;; gnc:html-table in "labeled amount" form.  IOW, it uses
;; gnc:html-table-add-labeled-amount-line!
;; 
;; The returned gnc:html-table is similar to what
;; gnc:html-build-acct-table used to (and still should) produce.
;; 
;; this function accepts the following additional parameters:
;; parent-account-balance-mode: 'immediate-bal 'recursive-bal ['omit-bal/#f]
;; zero-balance-display-mode: ['show-balance] 'omit-balance
;; multicommodity-mode: [#f] 'table/#t
;; rule-mode: #t [#f]  (not meant to affect subtotal rules)
;; 
(define (gnc:html-table-add-account-balances
         html-table  ;; can be #f to create a new table
         acct-table
         params)
  (let* ((num-rows (gnc:html-acct-table-num-rows acct-table))
	 (rownum 0)
	 (html-table (or html-table (gnc:make-html-table)))
	 (get-val (lambda (alist key)
		    (let ((lst (assoc-ref alist key)))
		      (if lst (car lst) lst))))
	 )
    
    (while (< rownum num-rows)
	   (let* ((env (append
			(gnc:html-acct-table-get-row-env acct-table rownum)
			params))
		  (acct (get-val env 'account))
		  (children (get-val env 'account-children))
		  (label (get-val env 'account-label))
		  (acct-name (get-val env 'account-name)) ;; for diagnostics...
		  (report-commodity  (get-val env 'report-commodity))
		  (exchange-fn (get-val env 'exchange-fn))
		  (account-cols (get-val env 'account-cols))
		  (logical-cols (get-val env 'logical-cols))
		  (label-cols (get-val env 'label-cols))
		  (logical-depth (get-val env 'logical-depth))
		  (display-depth (get-val env 'display-depth))
		  (display-tree-depth (get-val env 'display-tree-depth))
		  (subtotal-mode (get-val env 'subtotal-mode))
		  (row-type (get-val env 'row-type))
		  (rule-mode (and (equal? row-type 'subtotal-row)
				  (get-val env 'rule-mode)))
		  (row-markup (and (equal? row-type 'subtotal-row)
				   "primary-subheading"))
		  (multicommodity-mode (get-val env 'multicommodity-mode))
		  (limit-behavior
		   (or (get-val env 'depth-limit-behavior)
		       'summarize))
		  (parent-acct-bal-mode
		   (or (get-val env 'parent-account-balance-mode)
		       'omit-bal))
		  (bal-method
		   ;; figure out how to calculate our balance:
		   ;; 'immediate-bal|'recursive-bal ('omit-bal handled below)
		   (or (and (equal? row-type 'subtotal-row) 'recursive-bal)
		       (and (equal? (+ display-depth 1) display-tree-depth)
			    (or (and (equal? limit-behavior 'summarize)
				     'recursive-bal)
				(and (null? children) 'immediate-bal)
				;; otherwise, parent account at depth limit,
				;; with either 'truncate or 'flatten...
				parent-acct-bal-mode
				)
			    )
		       (if (null? children) #f parent-acct-bal-mode)
		       'immediate-bal
		       )
		   )
		  (comm-amt
		   ;; this will be the immediate/recursive commodity
		   ;; balance (a commodity collector) or #f.
		   (get-val env
			    (car (or (assoc-ref
				      '((immediate-bal account-bal)
					(recursive-bal recursive-bal)
					(omit-bal #f))
				      bal-method)
				     '(#f)
				     ))))
		  (zero-mode (let ((mode
				       (get-val
					env 'zero-balance-display-mode)))
			       (or (if (equal? mode #t) 'show-balance mode)
				   'show-balance)
			       ))
		  (reverse-balance (gnc-reverse-balance acct))
		  (native-comm?
		   (lambda (amt)
		     (gnc:uniform-commodity? amt report-commodity)))
                  ;; amount is either a <gnc:monetary> or #f
		  (amount (and comm-amt
			       (if (and (equal? zero-mode 'omit-balance)
                                        (gnc-commodity-collector-allzero?
                                         comm-amt)
                                        )
				   #f
				   ;; else:
                                   ;; this let* block evals to a <gnc:monetary>
				   (let*
				       ((amt (gnc:make-commodity-collector)))
				     (if reverse-balance
					 (amt 'minusmerge comm-amt #f)
					 (set! amt comm-amt))
				     (or (and (native-comm? amt)
					      (gnc:sum-collector-commodity
					       amt
					       report-commodity
					       exchange-fn)
					      )
					 ((if (and (equal?
                                                    multicommodity-mode 'table)
                                                   (equal?
                                                    row-type 'account-row)
                                                   )
                                              gnc-commodity-table
                                              gnc:sum-collector-commodity
                                              )
                                          amt
                                          report-commodity
                                          exchange-fn
                                          )  ;; factored from below
; 					 (if (and (equal?
; 						   multicommodity-mode 'table)
; 						  (equal?
; 						   row-type 'account-row)
; 						  )
; 					     (gnc-commodity-table
; 					      amt
; 					      report-commodity
; 					      exchange-fn)
; 					     (gnc:sum-collector-commodity
; 					      amt
; 					      report-commodity
; 					      exchange-fn)
; 					     )

					 )
				     ) ;; end of let*
				   ) ;; end of if
			       ))
		  (indented-depth (get-val env 'indented-depth))
		  (account-colspan (get-val env 'account-colspan))
		  )

             ;; for each row do:

	     (gnc:html-table-add-labeled-amount-line!
	      html-table
	      (+ account-cols logical-cols) ;; table-width
	      row-markup                    ;; row-markup
              rule-mode
	      label
              indented-depth
              account-colspan               ;; label-colspan
              #f                            ;; label-markup
	      amount
	      (+ account-cols (- 0 1)
		 (- logical-cols display-depth)
		 ;; account for 'immediate-bal parents displaying children
		 ;; NOTE: before you go mucking with this, BE ABSOLUTELY
		 ;; SURE you know what you're doing... i spent A LOT of
		 ;; time trying to make sure this is right. i know, in
		 ;; some reports, the output might look incorrect. but,
		 ;; if you think long and hard about it, i think you'll
		 ;; find the current treatment correct... i think. -DM-
		 (- 0 (if (if (null? children)
			      #f
			      (equal? bal-method 'immediate-bal))
			  1 0)
		    )
		 (if (equal? subtotal-mode 'canonically-tabbed) 1 0)
		 )                          ;; amount-depth
	      1                             ;; amount-colspan
              "number-cell"                 ;; amount-markup
              )

	     (set! rownum (+ rownum 1)) ;; increment rownum
	     )
	   ) ;; end of while
    html-table
    )
  )


(define (gnc:second-html-build-acct-table
         start-date end-date
         tree-depth show-subaccts? accounts
         start-percent delta-percent
         show-col-headers?
         show-total? get-total-fn
         total-name group-types? show-parent-balance? show-parent-total?
         show-other-curr? report-commodity exchange-fn show-zero-entries?)
  ;; THIS NEW FUNCTION DOES NOT IMPLEMENT SOME FEATURES OF THE OLD ONE
  ;; of these options: start-percent/delta-percent, the balance column
  ;; header, show-total?/get-total-fn/total-name, and group-types? are
  ;; presently unimplemented.  many of these functions are better left
  ;; to the renderer, anyway.  but if you *really* need them, you may
  ;; still use gnc:first-html-build-acct-table.
  (let* ((env (list
	       (list 'start-date start-date)
	       (list 'end-date end-date)
	       (list 'display-tree-depth tree-depth)
	       ;;(list 'progress-start-percent start-percent)
	       ;;(list 'progress-length-percent delta-percent)
	       (list 'column-header show-col-headers?)
	       (list 'parent-account-subtotal-mode show-parent-total?)
	       (list 'report-commodity report-commodity)
	       (list 'exchange-fn exchange-fn)
	       (list 'zero-balance-display-mode
		     (if show-zero-entries?
			 'show-balance
			 'omit-balance))
	       ))
	 (html-table (gnc:make-html-table))
	 (acct-table (gnc:make-html-acct-table/env/accts env accounts))
	 (params (list
		  (list 'parent-account-balance-mode
			(if show-parent-balance? 'immediate-bal))
		  ))
	 )
    (gnc:html-table-add-account-balances html-table acct-table params)
    html-table
    ))

;; END

