;;;; Preferences...

(use-modules (ice-9 slib))
(require 'sort)

;; (define gnc:*double-entry-restriction*
;;   (gnc:make-config-var
;;    "Determines how the splits in a transaction will be balanced. 
;;  The following values have significance:
;; 
;;    #f        anything goes
;; 
;;    'force    The sum of all splits in a transaction will be
;;              forced to be zero, even if this requires the
;;              creation of additional splits.  Note that a split
;;              whose value is zero (e.g. a stock price) can exist
;;              by itself. Otherwise, all splits must come in at 
;;              least pairs.
;; 
;;    'collect  splits without parents will be forced into a
;;              lost & found account.  (Not implemented)"
;;    (lambda (var value)
;;      (cond
;;       ((eq? value #f)
;;        (_gnc_set_force_double_entry_ 0)
;;        (list value))
;;       ((eq? value 'force)
;;        (_gnc_set_force_double_entry_ 1)
;;        (list value))
;;       ((eq? value 'collect)
;;        (gnc:warn
;;         "gnc:*double-entry-restriction* -- 'collect not supported yet.  "
;;         "Ignoring.")
;;        #f)
;;       (else
;;        (gnc:warn
;;         "gnc:*double-entry-restriction* -- " value " not supported.  Ignoring.")
;;        #f)))
;;    eq?
;;    #f))

;; We'd rather use a hash table for this, but until hash-for-each or
;; hash-keys is generally available, we can't...
(define gnc:*options-entries* '())

;; This will be an alist
;;   (k v) -> (section-name list-of-option-items)

(define (gnc:make-configuration-option
	 ;; The category of this option
         section
         name
	 ;; The sort-tag determines the relative ordering of options in
	 ;; this category. It is used by the gui for display.
         sort-tag
         type
         documentation-string
         getter
	 ;; The setter is responsible for ensuring that the value is valid.
         setter
         default-getter
         generate-restore-form
	 ;; Validation func should accept a value and return (#t value)
	 ;; on success, and (#f "failure-message") on failure. If #t,
	 ;; the supplied value will be used to set the option.
         value-validator)
  (vector section
          name
          sort-tag
          type
          documentation-string
          getter
          setter
          default-getter
          generate-restore-form
          value-validator))

(define (gnc:make-simple-boolean-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-value)
  (let ((option default-value))
    (gnc:make-configuration-option section name sort-tag 'boolean
				   documentation-string
				   (lambda () option)
				   (lambda (x) (set! option x))
				   (lambda () default-value)
				   #f
				   (lambda (x) (list #t x)))))

(define (gnc:configuration-option-section option)
  (vector-ref option 0))
(define (gnc:configuration-option-name option)
  (vector-ref option 1))
(define (gnc:configuration-option-sort-tag option)
  (vector-ref option 2))
(define (gnc:configuration-option-type option)
  (vector-ref option 3))
(define (gnc:configuration-option-documentation option)
  (vector-ref option 4))
(define (gnc:configuration-option-getter option)
  (vector-ref option 5))
(define (gnc:configuration-option-setter option)
  (vector-ref option 6))
(define (gnc:configuration-option-default-getter option)
  (vector-ref option 7))
(define (gnc:configuration-option-generate-restore-form option)
  (vector-ref option 8))
(define (gnc:configuration-option-value-validator option)
  (vector-ref option 9))


(define (gnc:register-configuration-option new-item)

  (let* ((section (gnc:configuration-option-section new-item))
         (existing-entry (assoc-ref gnc:*options-entries* section)))
    (if existing-entry
        (set! gnc:*options-entries*
              (assoc-set! gnc:*options-entries*
                          section 
                          (cons new-item existing-entry)))
        (set! gnc:*options-entries*
              (assoc-set! gnc:*options-entries*
                          section 
                          (list new-item))))))

(define (gnc:send-ui-section-options section-info)
  ;; section-info is a pair (section-name . list-of-options)
  (for-each gnc:register-option-ui (cdr section-info)))

(define (gnc:send-ui-options)
  (for-each gnc:send-ui-section-options gnc:*options-entries*))


(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show bank accounts"
  "a" "Show bank accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show cash accounts"
  "b" "Show cash accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show credit accounts"
  "c" "Show credit accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show asset accounts"
  "d" "Show asset accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show liability accounts"
  "e" "Show liability accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show stock accounts"
  "f" "Show stock accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show mutual fund accounts"
  "g" "Show mutual fund accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show currency accounts"
  "h" "Show currency accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show income accounts"
  "i" "Show income accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show expense accounts"
  "j" "Show expense accounts in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Types" "Show equity accounts"
  "k" "Show equity accounts in the account tree." #t))


(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account name"
  "a" "Show the account name column in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account type"
  "b" "Show the account type column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account code"
  "c" "Show the account code column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account description"
  "d" "Show the account description column in the account tree." #t))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account notes"
  "e" "Show the account notes column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account currency"
  "f" "Show the account currency column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account security"
  "g" "Show the account security column in the account tree." #f))

(gnc:register-configuration-option
 (gnc:make-simple-boolean-option
  "Account Fields" "Show account balance"
  "h" "Show the account balance column in the account tree." #t))


(define gnc:*arg-show-usage*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*arg-show-help*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*config-dir*
  (gnc:make-config-var
   "Configuration directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_config-dir-default_))

(define gnc:*share-dir*
  (gnc:make-config-var
   "Shared files directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_share-dir-default_))

;; Convert the temporary startup value into a config var.
(let ((current-value gnc:*debugging?*))
  (set! 
   gnc:*debugging?*
   (gnc:make-config-var
    "Enable debugging code."
    (lambda (var value) (if (boolean? value) (list value) #f))
    eq?
    #f))
  (gnc:config-var-value-set! gnc:*debugging?* #f current-value))

;; Convert the temporary startup value into a config var.
(let ((current-load-path gnc:*load-path*))
  (set!
   gnc:*load-path*
   (gnc:make-config-var
    "A list of strings indicating the load path for (gnc:load name).
Each element must be a string representing a directory or a symbol
where 'default expands to the default path, and 'current expands to
the current value of the path."
    (lambda (var value)
      (let ((result (gnc:_expand-load-path_ value)))
        (if (list? result)
            (list result)
            #f)))
    equal?
    '(default)))
  (gnc:config-var-value-set! gnc:*load-path* #f current-load-path))

(define gnc:*doc-path*
  (gnc:make-config-var
   "A list of strings indicating where to look for html and parsed-html files
Each element must be a string representing a directory or a symbol
where 'default expands to the default path, and 'current expands to
the current value of the path."
   (lambda (var value)
     (let ((result (gnc:_expand-doc-path_ value)))
       (if (list? result)
           (list result)
           #f)))
   equal?
   '(default)))
