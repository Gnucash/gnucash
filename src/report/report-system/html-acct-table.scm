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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
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
;; and gnc:html-table-add-accts!  functions.  
;; 
;; The gnc:html-acct-table parameters, set with
;; gnc:make-html-acct-table/env and gnc:make-html-acct-table/accts/env
;; and fetched with gnc:html-acct-table-env; accept the following
;; parameters:
;; 
;;     display-tree-depth: integer 'unlimited 'all #f
;; 
;;         the number of levels of accounts to display
;;         'unlimited, 'all, and #f impose no depth limit.
;;         the default is 'all.
;; 
;;     depth-limit-behavior: 'summarize 'flatten 'truncate
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
;;         ocurred, yet.
;; 
;;     report-commodity: commodity
;; 
;;         the commodity into which to convert any balances containing
;;         foreign currencies.  the balance will be converted using
;;         the exchange function exchange-fn. the defalut is the
;;         currency returned by (gnc:default-report-currency).
;;
;;     exchange-fn: commodity_exchange_function
;; 
;;         the commodity exchange function (you know, that weighted
;;         average, most recent, nearest in time fun stuff) used to
;;         convert balances which are not exclusively in the report
;;         commodity into the report commodity.
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
;;         by gnc:account-get-code.
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
;;         selected account has a deselected ancestor.
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
;;         indent.
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
;;         row, as returned by gnc:account-get-commodity. the g-wrap
;;         documentation string reads: "Get the commodity in which the
;;         account is denominated." note: afaik, gnucash accounts can
;;         only contain one commodity; but it's plausible that future
;;         releases may permit mixed-commodity accounts, so it's
;;         probably safest not to assume that an account contains only
;;         its default commodity.
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
  (string<? (gnc:account-get-code a)
	    (gnc:account-get-code b)))
(define (gnc:account-name-less-p a b)
  (string<? (gnc:account-get-name a)
	    (gnc:account-get-name b)))
(define (gnc:account-path-less-p a b)
  (string<? (gnc:account-get-full-name a)
	    (gnc:account-get-full-name b)))

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
			    #f
			    lim)))
	 (limit-behavior (or (get-val env 'depth-limit-behavior) 'summarize))
	 (indent (or (get-val env 'initial-indent) 0))
	 (less-p (let ((pred (get-val env 'account-less-p)))
		   (if (equal? pred #t) gnc:account-code-less-p pred)))
	 (start-date (get-val env 'start-date))
	 (end-date (or (get-val env 'end-date)
		       (cons 'absolute (cons (current-time) 0))))
	 (report-commodity (or (get-val env 'report-commodity)
			       (gnc:default-report-currency)))
	 (exchange-fn (or (get-val env 'exchange-fn)
			  'weighted-average))
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
	 ;; local variables
	 (toplvl-accts (gnc:group-get-account-list (gnc:get-current-group)))
	 (acct-depth-reached 0)
	 (logi-depth-reached (if depth-limit (- depth-limit 1) 0))
	 (disp-depth-reached 0)
	 )
    
    (define (traverse-accounts! accts acct-depth logi-depth)
      
      (define (use-acct? acct)
	(and (or (equal? limit-behavior 'flatten) (< logi-depth depth-limit))
	     (member acct accounts)
	     )
	)
      
      ;; the following two functions were lifted directly
      ;; from html-utilities.scm
      (define (my-get-balance-nosub account start-date end-date)
	(if start-date
	    (gnc:account-get-comm-balance-interval
	     account start-date end-date #f)
	    (gnc:account-get-comm-balance-at-date
	     account end-date #f)))
      
      ;; Additional function that includes the subaccounts as
      ;; well. Note: It is necessary to define this here (instead of
      ;; changing an argument for account-get-balance) because the
      ;; use-acct? query is needed.
      (define (my-get-balance account start-date end-date)
	;; this-collector for storing the result
	(let ((this-collector
	       (my-get-balance-nosub account start-date end-date)))
	  (for-each
	   (lambda (x) (if x (gnc:commodity-collector-merge this-collector x)))
	   (gnc:group-map-all-accounts
	    (lambda (a)
	      ;; Important: Calculate the balance if and only if the
	      ;; account a is shown, i.e. (use-acct? a) == #t.
	      (and (use-acct? a)
		   (my-get-balance-nosub a start-date end-date)))
	    (gnc:account-get-children account)))
	  this-collector))
      
      (let ((disp-depth
	     (if (integer? depth-limit)
		 (min (- depth-limit 1) logi-depth)
		 logi-depth))
	    )
	
	(for-each
	 (lambda (acct)
	   (let* ((subaccts
		   (gnc:account-get-immediate-subaccounts acct))
		  ;; assign output parameters
		  (account acct)
		  (account-name (gnc:account-get-name acct))
		  (account-code (gnc:account-get-code acct))
		  (account-path (gnc:account-get-full-name acct))
		  (account-anchor (gnc:html-account-anchor acct))
		  (account-parent (gnc:account-get-parent-account acct))
		  (account-children subaccts)
		  (account-depth acct-depth)
		  (logical-depth logi-depth)
		  (account-commodity (gnc:account-get-commodity acct))
		  (account-bal (my-get-balance-nosub
				acct start-date end-date))
		  (recursive-bal
		   (my-get-balance acct start-date end-date))
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
		 (and (gnc:commodity-collector-allzero? recursive-bal)
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
	     ;; Dive into an account even if it isnt selected!
	     (traverse-accounts! subaccts
				 (+ acct-depth 1)
				 (if (use-acct? acct)
				     (+ logi-depth 1)
				     logi-depth)
				 )
	     (or (not (use-acct? acct))
		 (not subtotal-mode)
		 ;; ignore use-acct for subtotals...
		 ;; (not (use-acct acct))
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
	     ))
	 (if less-p
	     (sort accts less-p)
	     accts)
	 ))
      )
    
    ;; do it
    (traverse-accounts! toplvl-accts 0 0)
    
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
  (- (gnc:html-table-num-cols (gnc:_html-acct-table-matrix_ acct-table)) 1))

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

(define (gnc:identity i) i)

(define (gnc:html-acct-table-render acct-table doc)
  ;; this will be used if we ever decide to let the utility object
  ;; render a document by calling thunks registered in the row-envs...
  ;; but, for now, this (optional) feature is left unimplemented...
  #f
  )

;; 
;; Here are some standard functions to help process gnc:html-acct-tables.
;; 

;; Stylesheets define the following cell styles which these functions
;; use: "text-cell" "total-label-cell" "number-cell"
;; "total-number-cell".  Row styles include "normal-row",
;; "alternate-row", "primary-subheading", "secondary-subheading", and
;; "grand-total". there really should also be a "first-number-cell"
;; and "last-number-cell" to put currency symbols and underlines,
;; respectively, on the numbers.

(define (gnc:html-table-add-labeled-amount-line!
	 html-table table-width row-markup total-rule?
	 label label-depth label-colspan label-markup
	 amount amount-depth amount-colspan amount-markup)
  ;; function to add a label and/or amount (which we'll call a "line")
  ;; to a gnc:html-table. all depths are zero-indexed.
  ;; if total-rule?, an <hr> is placed in the cell previous to label
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
	   (gnc:html-make-empty-cells lbl-depth)
	   (list
	    (if label-markup
		(gnc:make-html-table-cell/size/markup
		 1 lbl-colspan label-markup label)
		(gnc:make-html-table-cell/size
		 1 lbl-colspan label))
	    )
	   (gnc:html-make-empty-cells
		(+ (- amt-depth (+ lbl-depth lbl-colspan))
		   (if total-rule? -1 0)
		   ))
	   (if total-rule?
	       (list (gnc:make-html-table-cell
		      (gnc:make-html-text (gnc:html-markup-hr))))
	       (list)
	       )
	   (list
	    (if amount-markup
		(gnc:make-html-table-cell/size/markup
		 1 amt-colspan amount-markup amount)
		(gnc:make-html-table-cell/size
		 1 amt-colspan amount))
	    )
	   (gnc:html-make-empty-cells
	    (- table-width (+ amt-depth amt-colspan)))
	   ))
	 )
    (if row-markup
	(gnc:html-table-append-row/markup! html-table row-markup row)
	(gnc:html-table-append-row! html-table row))))

(define (gnc:commodity-table amount report-commodity exchange-fn)
  ;; this creates a small two-column table listing each commodity
  ;; balance and its respective report balance.  note that this
  ;; shows report-commodity amounts twice: first as a commodity
  ;; and second in the report commodity.  though this may arguably
  ;; be a bit redundant, i beleive that it makes the report more
  ;; readable.
  (let* ((table (gnc:make-html-table))
	 )
    (gnc:commodity-collector-map
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
;; gnc:html-table in "labeled amount" form.
;; 
;; The resulting gnc:html-table is similar to what
;; gnc:html-build-acct-table used to (and still should) produce.
;; 
;; this function accepts the following additional parameters:
;; parent-account-balance-mode: 'immediate-bal 'recursive-bal ['omit-bal/#f]
;; zero-balance-display-mode: ['show-balance] 'omit-balance
;; multicommodity-mode: [#f] 'table/#t
;; rule-mode: #t [#f]
;; 
(define (gnc:html-table-add-account-balances html-table acct-table params)
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
		  (multicommodity-mode (get-val env 'multicommodity-mode))
		  (limit-behavior
		   (or (get-val env 'depth-limit-behavior)
		       'summarize))
		  (parent-acct-bal-mode
		   (or (get-val env 'parent-account-balance-mode)
		       'omit-bal))
		  (bal-method
		   ;; figure out how to calculate our balance:
		   ;; 'immediate-bal|'recursive-bal|'omit-bal
		   (or (and (equal? row-type 'subtotal-row) 'recursive-bal)
		       (and (equal? (+ display-depth 1) display-tree-depth)
			    (if (equal? limit-behavior 'truncate)
				'immediate-bal
				;; 'summarize, 'flatten, and unknown
				;; depth limit behaviors yield
				;; 'recursive-bal.  this is true
				;; whether a leaf account or not.
				'recursive-bal)
			    )
		       (if (null? children) #f parent-acct-bal-mode)
		       'immediate-bal
		       )
		   )
		  (comm-amt
		   ;; this will be the immediate/recursize commodity
		   ;; balance or #f
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
		  (reverse-balance (gnc:account-reverse-balance? acct))
		  (native-comm?
		   (lambda (amt)
		     (gnc:uniform-commodity? amt report-commodity)))
		  (amount (and comm-amt
			       (if (and (equal? zero-mode 'omit-balance)
				    (gnc:commodity-collector-allzero? comm-amt)
				    )
				   #f
				   ;; otherwise
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
					 (if (and (equal?
						   multicommodity-mode 'table)
						  (equal?
						   row-type 'account-row)
						  )
					     (gnc:commodity-table
					      amt
					      report-commodity 
					      exchange-fn)
					     (gnc:sum-collector-commodity
					      amt
					      report-commodity
					      exchange-fn)
					     )
					 )
				     )
				   )
			       ))
		  (indented-depth (get-val env 'indented-depth))
		  (account-colspan (get-val env 'account-colspan))
		  )
	     (gnc:html-table-add-labeled-amount-line!
	      html-table
	      (+ account-cols logical-cols)
	      #f rule-mode
	      label indented-depth account-colspan #f ;"label-cell"
	      amount
	      (+ account-cols (- 0 1)
		 (- logical-cols display-depth)
		 ;; account for 'immediate-bal parents displaying children
		 ;; NOTE: before you go mucking with this, BE ABSOLUTELY
		 ;; SURE you know what youre doing... i spent A LOT of
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
		 )
	      1 "number-cell")
	     (set! rownum (+ rownum 1))
	     )
	   )
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

