;; text-export.scm
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

(gnc:support "text-export.scm")

(require 'pretty-print)
(gnc:depend "engine-utilities.scm")
(gnc:depend "srfi-1.scm")

;; TODO
;;
;; Eventually I think we should have a centralized description of what
;; all the data structs are and what's in them.  This would allow us
;; to automate the read/write procedure and make sure we don't get
;; skew.  For example, we should be able to say (something more
;; sophisticated than this):
;;
;; (define-data-contents "split"
;;   ("memo" 'string gnc:split-get-memo gnc:split-set-memo)
;;   ("share-amount" 'number d-gnc:split-get-share-amount ...)
;;   ...)
;;
;; and then autogenerate the input and output forms or something...
;; For now, we just hard-code everything...

;;; public bits
;;;
;;; Probably some of the other bits should be public, but we can add
;;; those once we decide...

(define gnc:account-group-write #f)
(define gnc:main-win-account-group-write #f)

;; Private scope for local-only bits...

(let ()

  (define (write-data form . port)
    ;;(apply pretty-print form port))
    (apply write form port))

  (define (engine-date->editable-date engine-date)
    (list (strftime "%a, %d %b %Y %H:%M:%S %z" (localtime (car engine-date)))
          (cdr engine-date)))

  (define (gnc:account-get-id account)
    ;; FIXME: dummy func to be used until I fix up the rest of this code.
    #f)

  (define (gnc:account-get-acc-info account)
    ;; FIXME: dummy func to be used until I fix up the rest of this code.
    #f)

  (define (gnc:cast-to-inv-acct account)
    ;; FIXME: dummy func to be used until I fix up the rest of this code.
    #f)

  (define (gnc:inv-acct-get-price-src account)
    ;; FIXME: dummy func to be used until I fix up the rest of this code.
    #f)

  (define (generate-account-chart group)
    ;; This should generate a form describing the hierarchical
    ;; structure of the accounts in the group.  It is only intended to
    ;; convey the overal hierarchy, not the account information.  As
    ;; such, it only contains the account name, the engine integer ID,
    ;; and the account guid.

    (define (handle-account account)
      (let ((name (gnc:account-get-name account))
            (id (gnc:account-get-id account)))
        
        (list id name
              (gnc:group-map-accounts
               handle-account
               (gnc:account-get-children account)))))
    
    (cons 'chart-of-accounts
          (gnc:group-map-accounts handle-account group)))


  (define (split->output-form split)
    
    ;; An alist for the split whose value is an alist for the data.
    (list
     'split
     (list 'guid (gnc:split-get-guid split))
     (list 'memo (gnc:split-get-memo split))
     (list 'action (gnc:split-get-action split))
     (list 'reconcile-state (gnc:split-get-reconcile-state split))
     (list 'reconciled-date 
           (engine-date->editable-date (gnc:split-get-reconciled-date split)))
     (list 'share-amount (d-gnc:split-get-share-amount split))
     (list 'share-price (d-gnc:split-get-share-price split))
     (list 'account
           (let ((xfer-account (gnc:split-get-account split))
                 (xfer-account-id #f))
             (if xfer-account
                 (set! xfer-account-id (gnc:account-get-id xfer-account)))
             xfer-account-id))))
  
  (define (txn->output-form transaction)
    (list
     'transaction
     (list 'guid (gnc:transaction-get-guid transaction))
     (list 'num (gnc:transaction-get-num transaction))
     (list 'date-posted
           (engine-date->editable-date 
            (gnc:transaction-get-date-posted transaction)))
     (list 'date-entered
           (engine-date->editable-date 
            (gnc:transaction-get-date-entered transaction)))
     (list 'description (gnc:transaction-get-description transaction))
     (cons 'splits
           (gnc:transaction-map-splits split->output-form transaction))))
  
  (define (account-info->output-form a)
    (let* ((accinfo (gnc:account-get-acc-info a))
           (invacct (gnc:cast-to-inv-acct accinfo)))
      (if invacct
          (gnc:inv-acct-get-price-src invacct)
          #f)))

  (define (account->output-form a)
    (list
     'account
     (list 'guid (gnc:account-get-guid a))
     (list 'name (gnc:account-get-name a))
     (list 'type (gnc:account-get-type-string a))
     (list 'code (gnc:account-get-code a))
     (list 'description (gnc:account-get-description a))
     (list 'notes (gnc:account-get-notes a))
     (list 'currency (gnc:account-get-currency a))
     (list 'security (gnc:account-get-security a))
     (list 'price-source (account-info->output-form a))))
  
  (define (account-txns-write account . port)
    (gnc:account-staged-transaction-traversal
     account
     1
     (lambda (t)
       (apply newline port)
       (apply newline port)
       (apply write-data (txn->output-form t) port)
       #f)))

  ;;; Public bits.

  (define (account-group-write account-group . port)

    ;; So we don't have to use apply everywhere...
    (if (null? port)
        (set! port (current-output-port))
        (set! port (car port)))

    ;; Export format meta-info: version, etc.
    (display "\
;;;;;;-*-scheme-*-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Welcome to the GnuCash text storage format.  This file is
;;; comprised of three sections.
;;;
;;; The first section is the chart of accounts.  Here the overall
;;; hierarchy of your accounts is recorded.  You may rearrange this
;;; hierarchy, but please don't edit any of the per-account
;;; information, unless you're just changing the name to match changes
;;; in the corresponding account information section.
;;;
;;; The second section contains all of the account information.  This
;;; is a sequence of forms describing each of the accounts given IDs
;;; in the chart of accounts.
;;;
;;; The final section contains all of your transactions as a sequence
;;; of forms.
;;;
;;; General notes:
;;;
;;; Dates are represented as lists of two elements: (seconds
;;; nanoseconds) where seconds is a localized date string and
;;; nanoseconds is an integer.  For example:
;;;
;;;   (\"Sat, 25 Oct 1997 11:00:00 +0500\" 0))
;;;

" port)           
    (write-data '(gnucash-data-file-version 2) port)
    
    (newline port)
    (display "\n;;; Chart of accounts (account hierarchy)." port)
    (display "\n;;; Each account is listed as (id name children)," port)
    (display "\n;;; and changes to the names here are ignored." port)
    (display "\n;;; Change the names in the account info section" port)
    (display      " below." port)
    (newline port)
    (write-data (generate-account-chart account-group) port)
    
    (newline port)
    (display "\n;;; Account information.\n" port)
    (map
     (lambda (account)
       (newline port)
       (write-data account port))
     (gnc:group-map-all-accounts account->output-form account-group))
    
    (display "\n\n;;; Transactions\n\n" port)
    ;; Now print all the transactions
    (gnc:group-begin-staged-transaction-traversals account-group)

    ;;(gnc:group-map-accounts
    ;; (lambda (account)
    ;;   (newline port)
    ;;   (account-txns-write account port))
    ;; account-group)
    
    (gnc:group-map-all-accounts
     (lambda (account)
       (newline port)
       (account-txns-write account port))
     account-group)

    )
  
  (define (main-win-account-group-write win)
    (let ((account-group (gnc:get-current-group)))
      (if (not account-group)
          (gnc:error-dialog "No account group available for text export.")
          (gnc:account-group-write account-group))))

  (set! gnc:account-group-write account-group-write)  
  (set! gnc:main-win-account-group-write main-win-account-group-write))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Import code

;; NOTE: This is possibly all wrong right now.  I think I'd like a
;; more modular restricted parser that all this should be based on,
;; but I wanted to get something up and running quickly, so I've
;; hard-coded things here.  The future implementation should allow you
;; to do things like this:
;;
;;   (gnc:rxp-add-token parser '+ add-safely)
;;
;; but that'll have to wait for a bit.
;;
;; Also, the error checking and diagnostic output needs to be
;; improved.  It's fairly primitive now, but we'll fix it up over
;; time.

;; Right now, the error handling convention is that every function
;; takes a "status" hash-table argument that will be modified to
;; reflect any errors.  Most of the time, when there's an error, this
;; hash will only contain a single string associated with the key
;; 'message.  Using a modifiable argument is done so that the return
;; semantics of returning #f on failure can be preserved.  This makes
;; the code cleaner, even if it's a less functional style.  If someone
;; has a better alternative, I'd be happy to entertain it.

(define gnc:account-group-import-from-text-file #f)

(let ()
  
  (define (msg-str . args)
    (call-with-output-string
     (lambda (port)
       (for-each (lambda (x)
                   (cond
                    ((string? x) (display x port))
                    ((and (list? x) (eq? (car x) 'w)) (write x port))
                    ((and (list? x) (eq? (car x) 'd)) (display x port))))))))

  (define (expect-file-version! form result-data)
    ;; returns (cons #t version) on success, or (cons #f message) on
    ;; failure.  On success also sets value for 'version in
    ;; result-data hash to be the version number.
    (cond
     ((not (and (list? form)
                (= (length form) 2)
                (eq? (car form) 'gnucash-data-file-version)
                (integer? (cadr form))))
      (cons #f
            (msg-str "expected (gnucash-data-file-version N), got: "
                     `(w ,form))))
     (else
      (hashq-set! result-data 'version (cadr form))
      (cons #t (cadr form)))))
  
  (define (handle-chart-of-accounts! form data)    
    ;; Returns (cons #t chart) on success, or (cons #f message) on
    ;; failure.
    
    (define (valid-chart-member? member)
      (and (list? member)
           (= (length member) 3)
           (integer? (first member))
           (string? (second member))
           (every valid-chart-member? (third member))))
    
    (define (valid-chart-contents? contents)
      (or (null? contents)
          (and (list? contents)
               (every valid-chart-member? contents))))
    
    (cond
     ((and (list? form)
           (>= (length form) 1)
           (eq? (car form) 'chart-of-accounts)
           (valid-chart-contents? (cdr form)))
      (hashq-set! data 'chart-of-accounts (cdr form))
      (cons #t (cdr form)))
     (else
      (cons #f (msg-str "bad chart of accounts: " `(w ,form))))))

  (define (handle-1-arg-form form arg-type? type-name hash hash-id)
    ;; By this point, we know that the form is a pair and that the
    ;; first element is the correct symbol, but we have to check
    ;; everything else.
    ;;
    ;; This function returns (cons #t '()) on success, and (cons #f
    ;; msg) otherwise.
    (cond
     ((not (and (list? form) (= (length form) 2)))
      (cons #f (msg-str "expected field with one argument, got: "
                        `(w ,form))))
     ((not (arg-type? (cadr form)))
      (cons #f (msg-str "expected field arg of type " type-name ", got: "
                        `(w ,form))))
     (else
      (let ((old-key-val (hashq-get-handle hash hash-id)))
        (if old-key-val
            (cons #f (msg-str "duplicate "
                              hash-id " field in account form: "
                              `(w ,form)))
            (begin
              (hashq-set! hash hash-id (cadr form))
              (cons #t '())))))))
      
  (define (handle-account! form)
    ;; At this point we know that form is at least a pair, and the
    ;; first element is 'account, but that's it.
    ;;
    ;; If everything checks out, returns (#t account-info), otherwise
    ;; returns (cons #f error-description-string).  If returned,
    ;; account-info will be a hash containing the relevant account
    ;; data, but no checking is done here for deeper semantic issues.
    ;; Note too, that invalid account forms will cause this function
    ;; to fail.  That includes duplicate or missing fields.

    ;; For now, all the account fields are required, but the order is
    ;; irrelevant.

    (let ((acc-info (make-hash-table 7)))
      
      (define (handle-account-field field-form)
        ;; This is going to be called by "any", so it must return #f
        ;; on success.

        (cond
         ((not (and (list? field-form)
                    (>= (length field-form) 1)
                    (symbol? (car field-form))))
          (cons #f (msg-str "bad field in account form: " `(w field-form))))
         (else
          (let* ((id (car field-form))
                 (result
                  (case id
                    ((id)
                     (handle-1-arg-form field-form
                                        integer? "integer" acc-info id))
                    ((name)
                     (handle-1-arg-form field-form
                                        string? "string" acc-info id))
                    ((flags)
                     (handle-1-arg-form field-form
                                        char? "character" acc-info id))
                    ((type)
                     (handle-1-arg-form field-form
                                        symbol? "symbol" acc-info id))
                    ((code)
                     (handle-1-arg-form field-form
                                        string? "string" acc-info id))
                    ((description)
                     (handle-1-arg-form field-form
                                        string? "string" acc-info id))
                    ((notes)
                     (handle-1-arg-form field-form
                                        string? "string" acc-info id))
                    ((currency)
                     (handle-1-arg-form field-form
                                        string? "string" acc-info id))
                    ((security)
                     (handle-1-arg-form field-form
                                        string? "string" acc-info id))
                    ((price-source)
                     (handle-1-arg-form field-form
                                        (lambda (v)
                                          (or (not v)
                                              (integer? v)))
                                        "integer or #f" acc-info id))
                    (else
                     (cons #f
                           (msg-str "unknown field name in account form:"
                                    `(w ,field-form)))))))
            
            (if (car result)
                #f
                result)))))
      
      (cond
       ((not (list? form))
        (cons #f (msg-str "bad account form; not a list: " `(w ,form))))
       ((= (length form) 11)
        (cons #f (msg-str "bad account form; wrong number of elements: "
                          `(w ,form))))
       (else
        (let ((result (any handle-account-field (cdr form))))
          (if (not result)
              (let ((prev-accounts
                     (let ((v (hashq-ref acc-info "accounts")))
                       (or v '()))))
                ;; parsing went OK, so use the results.
                (hashq-set! acc-info "accounts" (cons acc-info prev-accounts))
                (cons #t acc-info))
              ;; Failure.  result should be of the form (cons #f err-msg)
              result))))))
              
  (define (handle-transaction! form data)
    #t)

  ;;; FIXME: this setup allows multiple charts of accounts, and
  ;;; doesn't yet require one.
  
  (define (parse-remainder! port data)
    (let loop ((next-form (read port)))
      (if (eof-object? next-form)
          #t                            ; There don't have to be any...
          (and (list? next-form)
               (symbol? (car next-form))
               (case (car next-form)
                 ((chart-of-accounts)
                  (handle-chart-of-accounts! next-form data))
                 ((account)
                  (handle-account! next-form data))
                 ((transaction)
                  (handle-transaction! next-form data))
                 (else
                  (display "Bad. Bad. Bad.\n")
                  #f))
               (loop (read port))))))

  (define (data->account-group data)
    (display "XXX: ")
    (display data)
    (newline)
    (display "XXX: ") (display 'version) (display " ")
    (display ((record-accessor import-data-type 'version) data))
    (newline)
    (cons #t #f))

  (define (port->account-group port)
    ;; This function returns (cons #f message) on failure, and (cons
    ;; #t AccountGroup*) on success.
    (let ((data (make-hash-table 7))
          (result '(#t)))
      
      ;; This should be restructured so that the parser is determined
      ;; once we know the version...
      
      (and 
       (begin (set! result (expect-file-version! (read-port) data))
              (car result))
       (begin (set! result (parse-remainder! (read-port) data))
              (car result))
       (begin (set! result (deeper-issues-ok? data))
              (car result))
       (begin (set! result (data->account-group data))
              (car result)))

      result))
      
  (define (import-from-file filename)
    (call-with-input-file filename port->account-group))

    
  (set! gnc:account-group-import-from-text-file import-from-file))

;     (let loop ((next-form (read port))
;                (data-file-version #f))
;       (if (not (eof-object? next-form))
;           (begin
;             (display "XXX: ")
;             (display next-form)
;             (newline)
;             (loop (read port))))))
