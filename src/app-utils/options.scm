;; Scheme code for supporting options
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

(define (gnc:make-option
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
         ;; Restore form generator should generate an ascii representation
         ;; of a function taking one argument. The argument will be an
         ;; option. The function should restore the option to the original
         ;; value.
         generate-restore-form
         ;; the scm->kvp and kvp->scm functions should save and load
         ;; the option to a kvp.  The arguments to these function will be
         ;; a kvp-frame and a base key-path list for this option.
         scm->kvp
         kvp->scm
         ;; Validation func should accept a value and return (#t value)
         ;; on success, and (#f "failure-message") on failure. If #t,
         ;; the supplied value will be used by the gui to set the option.
         value-validator
         ;;; free-form storage depending on type.
         option-data 
         ;; If this is a "multiple choice" type of option,
         ;; this should be a vector of the following five functions:
         ;; 
         ;; Function 1: taking no arguments, giving the number of choices
         ;;
         ;; Function 2: taking one argument, a non-negative integer, that
         ;; returns the scheme value (usually a symbol) matching the
         ;; nth choice
         ;;
         ;; Function 3: taking one argument, a non-negative integer,
         ;; that returns the string matching the nth choice
         ;;
         ;; Function 4: takes one argument and returns the description
         ;; containing the nth choice
         ;;
         ;; Function 5: giving a possible value and returning the index
         ;; if an option doesn't use these,  this should just be a #f
         option-data-fns
         ;; This function should return a list of all the strings
         ;; in the option other than the section, name, (define
         ;; (list-lookup list item) and documentation-string that
         ;; might be displayed to the user (and thus should be
         ;; translated).
         strings-getter
         ;; This function will be called when the GUI representation
         ;; of the option is changed.  This will normally occur before
         ;; the setter is called, because setters are only called when
         ;; the user selects "OK" or "Apply".  Therefore, this
         ;; callback shouldn't be used to make changes to the actual
         ;; options database.
         option-widget-changed-proc)
  (let ((changed-callback #f))
    (vector section
            name
            sort-tag
            type
            documentation-string
            getter            
            (lambda args
              (apply setter args)
              (if changed-callback (changed-callback)))
            default-getter
            generate-restore-form
            scm->kvp
            kvp->scm
            value-validator
            option-data
            option-data-fns
            (lambda (callback) (set! changed-callback callback))
            strings-getter
            option-widget-changed-proc)))

(define (gnc:option-section option)
  (vector-ref option 0))
(define (gnc:option-name option)
  (vector-ref option 1))
(define (gnc:option-sort-tag option)
  (vector-ref option 2))
(define (gnc:option-type option)
  (vector-ref option 3))
(define (gnc:option-documentation option)
  (vector-ref option 4))
(define (gnc:option-getter option)
  (vector-ref option 5))
(define (gnc:option-setter option)
  (vector-ref option 6))
(define (gnc:option-default-getter option)
  (vector-ref option 7))
(define (gnc:option-generate-restore-form option)
  (vector-ref option 8))
(define (gnc:option-scm->kvp option)
  (vector-ref option 9))
(define (gnc:option-kvp->scm option)
  (vector-ref option 10))
(define (gnc:option-value-validator option)  
  (vector-ref option 11))
(define (gnc:option-data option)
  (vector-ref option 12))
(define (gnc:option-data-fns option)
  (vector-ref option 13))

(define (gnc:option-set-changed-callback option callback)
 (let ((cb-setter (vector-ref option 14)))
    (cb-setter callback)))
(define (gnc:option-strings-getter option)
  (vector-ref option 15))
(define (gnc:option-widget-changed-proc option)
  (vector-ref option 16))

(define (gnc:option-value option)
  (let ((getter (gnc:option-getter option)))
    (getter)))

(define (gnc:option-set-value option value)
  (let ((setter (gnc:option-setter option)))
    (setter value)))

(define (gnc:option-index-get-name option index)
  (let* ((option-data-fns (gnc:option-data-fns option))
         (name-fn (vector-ref option-data-fns 2)))
    (name-fn index)))

(define (gnc:option-index-get-description option index)
  (let* ((option-data-fns (gnc:option-data-fns option))
         (name-fn (vector-ref option-data-fns 3)))
    (name-fn index)))

(define (gnc:option-index-get-value option index)
  (let* ((option-data-fns (gnc:option-data-fns option))
         (name-fn (vector-ref option-data-fns 1)))
    (name-fn index)))

(define (gnc:option-value-get-index option value)
  (let* ((option-data-fns (gnc:option-data-fns option))
         (name-fn (vector-ref option-data-fns 4)))
    (name-fn value)))

(define (gnc:option-number-of-indices option)
  (let* ((option-data-fns (gnc:option-data-fns option))
         (name-fn (vector-ref option-data-fns 0)))
    (name-fn)))

(define (gnc:option-default-value option)
  (let ((getter (gnc:option-default-getter option)))
    (getter)))


(define (gnc:restore-form-generator value->string)
  (lambda () (string-append
              "(lambda (option) "
              "(if option ((gnc:option-setter option) "
              (value->string)
              ")))")))

(define (gnc:value->string value)
  (call-with-output-string
   (lambda (port) (write value port))))

(define (gnc:make-string-option
         section
         name
         sort-tag
         documentation-string
         default-value)
  (let* ((value default-value)
         (value->string (lambda () (gnc:value->string value))))
    (gnc:make-option
     section name sort-tag 'string documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value v))))
     (lambda (x)
       (cond ((string? x)(list #t x))
             (else (list #f "string-option: not a string"))))
     #f #f #f #f)))

(define (gnc:make-text-option
         section
         name
         sort-tag
         documentation-string
         default-value)
  (let* ((value default-value)
         (value->string (lambda () (gnc:value->string value))))
    (gnc:make-option
     section name sort-tag 'text documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value v))))
     (lambda (x)
       (cond ((string? x)(list #t x))
             (else (list #f "text-option: not a string"))))
     #f #f #f #f)))

;;; font options store fonts as strings a la the X Logical
;;; Font Description. You should always provide a default
;;; value, as currently there seems to be no way to go from
;;; an actual font to a logical font description, and thus
;;; there is no way for the gui to pick a default value.

(define (gnc:make-font-option
         section
         name
         sort-tag
         documentation-string
         default-value)
  (let* ((value default-value)
         (value->string (lambda () (gnc:value->string value))))
    (gnc:make-option
     section
     name
     sort-tag
     'font
     documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)     
     (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value v))))
     (lambda (x)
       (cond ((string? x)(list #t x))
             (else (list #f "font-option: not a string"))))
     #f #f #f #f)))

;; currency options use a specialized widget for entering currencies
;; in the GUI implementation.
(define (gnc:make-currency-option
         section
         name
         sort-tag
         documentation-string
         default-value)

  (define (currency->scm currency)
    (if (string? currency)
        currency
        (gnc-commodity-get-mnemonic currency)))

  (define (scm->currency currency)
    (if (string? currency)
        (gnc-commodity-table-lookup
         (gnc-commodity-table-get-table (gnc-get-current-book))
         GNC_COMMODITY_NS_CURRENCY currency)
        currency))

   (let* ((value (currency->scm default-value))
          (value->string (lambda () (gnc:value->string value))))
     (gnc:make-option
      section name sort-tag 'currency documentation-string
      (lambda ()  (scm->currency value))
      (lambda (x) (set! value (currency->scm x)))
      (lambda ()  (scm->currency default-value))
      (gnc:restore-form-generator value->string)
      (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
      (lambda (f p)
        (let ((v (kvp-frame-get-slot-path-gslist f p)))
          (if (and v (string? v))
              (set! value v))))
      (lambda (x) (list #t x))
      #f #f #f #f)))

;; budget option
;; TODO: need to double-check this proc
(define (gnc:make-budget-option
         section
         name
         sort-tag
         documentation-string)

  (define (budget->guid budget)
    (cond ((eq? budget #f) #f)
          ((string? budget) budget)
          (else (gncBudgetGetGUID budget))))

  (define (guid->budget budget)
    (if (string? budget)
        (gnc-budget-lookup budget (gnc-get-current-book))
        budget))

  (let* ((default-value (gnc-budget-get-default (gnc-get-current-book)))
         (value (budget->guid default-value))
         (option-set #f)
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))

         )
    (gnc:make-option
     section name sort-tag 'budget documentation-string
     ;; the getter should always return a budget pointer
     (lambda () (guid->budget  ;; getter
                 (if option-set
                     value
                     default-value))
             )

     (lambda (x) (set! value (budget->guid x))
             (set! option-set #t)) ;; setter
     (lambda ()
       (guid->budget
        (gnc-budget-get-default (gnc-get-current-book)))) ;; default-getter
     (gnc:restore-form-generator value->string) ;; ??
     (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value v))))
     (lambda (x) (list #t x)) ;; value-validator
     #f #f #f #f)))

;; commodity options use a specialized widget for entering commodities
;; in the GUI implementation.
(define (gnc:make-commodity-option
         section
         name
         sort-tag
         documentation-string
         default-value)

  (define (commodity->scm commodity)
    (if (string? commodity)
        (list 'commodity-scm
              GNC_COMMODITY_NS_CURRENCY
              commodity)
        (list 'commodity-scm
              (gnc-commodity-get-namespace commodity)
              (gnc-commodity-get-mnemonic commodity))))

  (define (scm->commodity scm)
    (gnc-commodity-table-lookup
     (gnc-commodity-table-get-table (gnc-get-current-book))
     (cadr scm) (caddr scm)))

   (let* ((value (commodity->scm default-value))
          (value->string (lambda ()
                           (string-append "'" (gnc:value->string value)))))
     (gnc:make-option
      section name sort-tag 'commodity documentation-string
      (lambda () (scm->commodity value))
      (lambda (x) (if (and (pair? x) (eqv? (car x) 'commodity-scm))
                      (set! value x)
                      (set! value (commodity->scm x))))
      (lambda () default-value)
      (gnc:restore-form-generator value->string)
      (lambda (f p) 
        (kvp-frame-set-slot-path-gslist f (cadr value) (append p '("ns")))
        (kvp-frame-set-slot-path-gslist f (caddr value) (append p '("monic"))))
      (lambda (f p)
        (let ((ns (kvp-frame-get-slot-path-gslist f (append p '("ns"))))
              (monic (kvp-frame-get-slot-path-gslist f (append p '("monic")))))
          (if (and ns monic (string? ns) (string? monic))
              (set! value (list 'commodity-scm ns monic)))))
      (lambda (x) (list #t x))
      #f #f #f #f)))


(define (gnc:make-simple-boolean-option
         section
         name
         sort-tag
         documentation-string
         default-value)
  (gnc:make-complex-boolean-option section
                                   name
                                   sort-tag
                                   documentation-string
                                   default-value
                                   #f 
                                   #f))

;; Complex boolean options are the same as simple boolean options (see
;; above), with the addition of two function arguments. (If both of
;; them are #f, you have exactly a simple-boolean-option.) Both
;; functions should expect one boolean argument.  When the option's
;; value is changed, the function option-widget-changed-cb will be
;; called with the new option value at the time that the GUI widget
;; representing the option is changed, and the function
;; setter-function-called-cb will be called when the option's setter
;; is called (that is, when the user selects "OK" or "Apply").

;; The option-widget-changed-cb is tested for procedurehood before
;; it is called, so it is not validated to be a procedure here.
;; However, since there could be an option-widget-changed-cb but not
;; a setter-function-called-cb, the procedurehood of the
;; setter-function-called-cb is checked here.
(define (gnc:make-complex-boolean-option
         section
         name
         sort-tag
         documentation-string
         default-value
         setter-function-called-cb
         option-widget-changed-cb)
  (let* ((value default-value)
         (value->string (lambda () (gnc:value->string value))))
    (gnc:make-option
     section name sort-tag 'boolean documentation-string
     (lambda () value)
     (lambda (x) (set! value x)
             (if (procedure? setter-function-called-cb)
                 (setter-function-called-cb x)))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (boolean? v) (not (equal? v default-value)))
             (set! value v))))
     (lambda (x)
       (if (boolean? x)
           (list #t x)
           (list #f "boolean-option: not a boolean")))
     #f #f #f (and option-widget-changed-cb
                   (lambda (x) (option-widget-changed-cb x))))))


(define (gnc:make-pixmap-option 
         section name sort-tag doc-string
         default-value)
  (let* ((value default-value))
    (gnc:make-option
     section name sort-tag 'pixmap doc-string
     (lambda ()  value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator  (lambda () (gnc:value->string value)))
     #f
     #f
     (lambda (x)
       (if (string? x)
           (begin 
             (list #t x))
           (begin 
             (list #f "pixmap-option: not a string"))))
     #f #f #f #f)))

;; show-time is boolean
;; subtype should be one of 'relative 'absolute or 'both
;; if subtype is 'absolute then relative-date-list should be #f
;; relative-date-list should be the list of relative dates permitted
;; gnc:all-relative-dates contains a list of all relative dates.

(define (gnc:make-date-option
         section
         name
         sort-tag 
         documentation-string
         default-getter
         show-time
         subtype
         relative-date-list)
  (define (date-legal date)
    (and (pair? date) 
         (or 
          (and (eq? 'relative (car date)) (symbol? (cdr date)))
          (and (eq? 'absolute (car date)) 
               (pair? (cdr date)) 
               (exact? (cadr date)) 
               (exact? (cddr date))))))
  (define (list-lookup list item)
    (cond
     ((null? list) #f)
     ((eq? item (car list)) 0)
     (else (+ 1 (list-lookup (cdr list) item)))))
  (let* ((value (default-getter))
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name sort-tag 'date documentation-string
     (lambda () value)
     (lambda (date)
       (if (date-legal date)
           (set! value date)
           (gnc:error "Illegal date value set:" date)))
     default-getter
     (gnc:restore-form-generator value->string)
     (lambda (f p)
       (kvp-frame-set-slot-path-gslist f (symbol->string (car value))
                                       (append p '("type")))
       (kvp-frame-set-slot-path-gslist f
                                       (if (symbol? (cdr value))
                                           (symbol->string (cdr value))
                                           (cdr value))
                                       (append p '("value"))))
     (lambda (f p)
       (let ((t (kvp-frame-get-slot-path-gslist f (append p '("type"))))
             (v (kvp-frame-get-slot-path-gslist f (append p '("value")))))
         (if (and t v (string? t))
             (set! value (cons (string->symbol t)
                               (if (string? v) (string->symbol v) v))))))
     (lambda (date)
       (if (date-legal date)
           (list #t date)
           (list #f "date-option: illegal date")))
     (vector subtype show-time relative-date-list) 
     (vector (lambda () (length relative-date-list))
             (lambda (x) (list-ref relative-date-list x))
             (lambda (x) (gnc:get-relative-date-string
                          (list-ref relative-date-list x)))
             (lambda (x) (gnc:get-relative-date-desc
                          (list-ref relative-date-list x)))
             (lambda (x) (list-lookup relative-date-list x)))
     #f
     #f)))

(define (gnc:get-rd-option-data-subtype option-data)
  (vector-ref option-data 0))

(define (gnc:get-rd-option-data-show-time option-data)
  (vector-ref option-data 1))

(define (gnc:get-rd-option-data-rd-list option-data)
  (vector-ref option-data 2))

(define (gnc:date-option-get-subtype option)
  (if (eq? (gnc:option-type option) 'date)
      (gnc:get-rd-option-data-subtype (gnc:option-data option))
      (gnc:error "Not a date option")))

(define (gnc:date-option-show-time? option)
  (if (eq? (gnc:option-type option) 'date)
      (gnc:get-rd-option-data-show-time (gnc:option-data option))
      (gnc:error "Not a date option")))

(define (gnc:date-option-value-type option-value)
  (car option-value))

(define (gnc:date-option-absolute-time option-value)
  (if (eq? (car option-value) 'absolute)
      (cdr option-value)
      (gnc:get-absolute-from-relative-date (cdr option-value))))

(define (gnc:date-option-relative-time option-value)
  (if (eq? (car option-value) 'absolute)
      #f
      (cdr option-value)))

;; Just like gnc:make-account-list-limited-option except it
;; does not limit the types of accounts that are available
;; to the user.
(define (gnc:make-account-list-option
         section
         name
         sort-tag
         documentation-string
         default-getter
         value-validator
         multiple-selection)

  (gnc:make-account-list-limited-option
   section name sort-tag documentation-string
   default-getter value-validator multiple-selection '()))

;; account-list options use the option-data as a pair; the car is
;; a boolean value, the cdr is a list of account-types. If the boolean is
;; true, the gui should allow the user to select multiple accounts.
;; If the cdr is an empty list, then all account types are shown.
;; Internally, values are always a list of guids. Externally, both
;; guids and account pointers may be used to set the value of the
;; option. The option always returns a list of account pointers.
(define (gnc:make-account-list-limited-option
         section
         name
         sort-tag
         documentation-string
         default-getter
         value-validator
         multiple-selection
         acct-type-list)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gncAccountGetGUID item)))

  (define (convert-to-account item)
    (if (string? item)
        (xaccAccountLookup item (gnc-get-current-book))
        item))

  (let* ((option (map convert-to-guid (default-getter)))
         (option-set #f)
         (getter (lambda () (map convert-to-account
                                 (if option-set
                                     option
                                     (default-getter)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (account-list) (list #t account-list))
              (lambda (account-list)
                (value-validator (map convert-to-account account-list))))))
    (gnc:make-option
     section name sort-tag 'account-list documentation-string getter
     (lambda (account-list)
       (if (or (not account-list) (null? account-list)) 
           (set! account-list (default-getter)))
       (set! account-list
             (filter (lambda (x) (if (string? x)
                                     (xaccAccountLookup
                                      x (gnc-get-current-book))
                                     x)) account-list))
       (let* ((result (validator account-list))
              (valid (car result))
              (value (cadr result)))
         (if valid
             (begin
               (set! option (map convert-to-guid value))
               (set! option-set #t))
             (gnc:error "Illegal account list value set"))))
     (lambda () (map convert-to-account (default-getter)))
     (gnc:restore-form-generator value->string)
     (lambda (f p)
       (define (save-acc list count)
         (if (not (null? list))
             (let ((key (string-append "acc" (gnc:value->string count))))
               (kvp-frame-set-slot-path-gslist f (car list) (append p (list key)))
               (save-acc (cdr list) (+ 1 count)))))

       (if option-set
           (begin
             (kvp-frame-set-slot-path-gslist f (length option)
                                             (append p '("len")))
             (save-acc option 0))))
     (lambda (f p)
       (let ((len (kvp-frame-get-slot-path-gslist f (append p '("len")))))
         (define (load-acc count)
           (if (< count len)
               (let* ((key (string-append "acc" (gnc:value->string count)))
                      (guid (kvp-frame-get-slot-path-gslist
                             f (append p (list key)))))
                 (cons guid (load-acc (+ count 1))))
               '()))
         
         (if (and len (integer? len))
             (begin
               (set! option (load-acc 0))
               (set! option-set #t)))))
     validator
     (cons multiple-selection acct-type-list) #f #f #f)))

;; Just like gnc:make-account-sel-limited-option except it
;; does not limit the types of accounts that are available
;; to the user.
(define (gnc:make-account-sel-option
         section
         name
         sort-tag
         documentation-string
         default-getter
         value-validator)

  (gnc:make-account-sel-limited-option
   section name sort-tag documentation-string
   default-getter value-validator '()))

;; account-sel options use the option-data as a pair; the car is
;; ignored, the cdr is a list of account-types. If the cdr is an empty
;; list, then all account types are shown.  Internally, the value is
;; always a guid.  Externally, both guids and account pointers may be
;; used to set the value of the option. The option always returns the
;; "current" account pointer.
(define (gnc:make-account-sel-limited-option
         section
         name
         sort-tag
         documentation-string
         default-getter
         value-validator
         acct-type-list)

  (define (convert-to-guid item)
    (if (string? item)
        item
        (gncAccountGetGUID item)))

  (define (convert-to-account item)
    (if (string? item)
        (xaccAccountLookup item (gnc-get-current-book))
        item))

  (define (find-first-account)
    (define (find-first account-list)
      (if (null? account-list)
          '()
          (let* ((this-account (car account-list))
                 (account-type (xaccAccountGetType this-account)))
            (if (if (null? acct-type-list)
                    #t
                    (member account-type acct-type-list))
                this-account
                (find-first (cdr account-list))))))

    (let* ((current-root (gnc-get-current-root-account))
           (account-list (gnc-account-get-descendants-sorted current-root)))
      (find-first account-list)))
  
  (define (get-default)
    (if default-getter
        (default-getter)
        (find-first-account)))

  (let* ((option (convert-to-guid (get-default)))
         (option-set #f)
         (getter (lambda () (convert-to-account
                             (if option-set
                                 option
                                 (get-default)))))
         (value->string (lambda ()
                          (string-append
                           "'" (gnc:value->string (if option-set option #f)))))
         (validator
          (if (not value-validator)
              (lambda (account) (list #t account))
              (lambda (account)
                (value-validator (convert-to-account account))))))
    (gnc:make-option
     section name sort-tag 'account-sel documentation-string getter
     (lambda (account)
       (if (or (not account) (null? account)) (set! account (get-default)))
       (set! account (convert-to-account account))
       (let* ((result (validator account))
              (valid (car result))
              (value (cadr result)))
         (if valid
             (begin
               (set! option (convert-to-guid value))
               (set! option-set #t))
             (gnc:error "Illegal account value set"))))
     (lambda () (convert-to-account (get-default)))
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f value p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value v))))
     validator
     (cons #f acct-type-list) #f #f #f)))

(define (gnc:multichoice-list-lookup list item )
  (cond
   ((null? list) #f)
   ((eq? item (vector-ref (car list) 0)) 0)
   (else (+ 1 (gnc:multichoice-list-lookup (cdr list) item)))))

;; multichoice options use the option-data as a list of vectors.
;; Each vector contains a permissible value (scheme symbol), a
;; name, and a description string.
(define (gnc:make-multichoice-option
         section
         name
         sort-tag
         documentation-string
         default-value
         ok-values)
  (gnc:make-multichoice-callback-option section
                                        name
                                        sort-tag
                                        documentation-string
                                        default-value
                                        ok-values
                                        #f
                                        #f))

;; The multichoice-option with callback function is the same as the
;; usual multichoice options (see above), with the addition of two
;; function arguments. (If both of them are #f, you have exactly a
;; multichoice-option.) Both functions should expect one argument.
;; When the option's value is changed, the function
;; option-widget-changed-cb will be called with the new option value
;; at the time that the GUI widget representing the option is changed,
;; and the function setter-function-called-cb will be called when the
;; option's setter is called (that is, when the user selects "OK" or
;; "Apply").
(define (gnc:make-multichoice-callback-option
         section
         name
         sort-tag
         documentation-string
         default-value
         ok-values
         setter-function-called-cb
         option-widget-changed-cb)
  (define (multichoice-legal val p-vals)
    (cond ((null? p-vals) #f)
          ((eq? val (vector-ref (car p-vals) 0)) #t)
          (else (multichoice-legal val (cdr p-vals)))))

  (define (multichoice-strings p-vals)
    (if (null? p-vals)
        ()
        (cons (vector-ref (car p-vals) 1)
              (cons (vector-ref (car p-vals) 2)
                    (multichoice-strings (cdr p-vals))))))

  (let* ((value default-value)
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name sort-tag 'multichoice documentation-string
     (lambda () value)
     (lambda (x)
       (if (multichoice-legal x ok-values)
           (begin
             (set! value x)
             (if (procedure? setter-function-called-cb)
                 (setter-function-called-cb x)))
           (gnc:error "Illegal Multichoice option set")))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f (symbol->string value) p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value (string->symbol v)))))
     (lambda (x)
       (if (multichoice-legal x ok-values)
           (list #t x)
           (list #f "multichoice-option: illegal choice")))
     ok-values
     (vector (lambda () (length ok-values))
             (lambda (x) (vector-ref (list-ref ok-values x) 0))
             (lambda (x) (vector-ref (list-ref ok-values x) 1))
             (lambda (x) (vector-ref (list-ref ok-values x) 2))
             (lambda (x)
               (gnc:multichoice-list-lookup ok-values x)))
     (lambda () (multichoice-strings ok-values)) 
     (and option-widget-changed-cb
          (lambda (x) (option-widget-changed-cb x))))))


;; radiobutton options use the option-data as a list of vectors.
;; Each vector contains a permissible value (scheme symbol), a
;; name, and a description string.
(define (gnc:make-radiobutton-option
         section
         name
         sort-tag
         documentation-string
         default-value
         ok-values)
  (gnc:make-radiobutton-callback-option section
                                        name
                                        sort-tag
                                        documentation-string
                                        default-value
                                        ok-values
                                        #f
                                        #f))

;; The radiobutton-option with callback function is the same as the
;; usual radiobutton options (see above), with the addition of two
;; function arguments. (If both of them are #f, you have exactly a
;; radiobutton-option.) Both functions should expect one argument.
;; When the option's value is changed, the function
;; option-widget-changed-cb will be called with the new option value
;; at the time that the GUI widget representing the option is changed,
;; and the function setter-function-called-cb will be called when the
;; option's setter is called (that is, when the user selects "OK" or
;; "Apply").
(define (gnc:make-radiobutton-callback-option
         section
         name
         sort-tag
         documentation-string
         default-value
         ok-values
         setter-function-called-cb
         option-widget-changed-cb)
  (define (radiobutton-legal val p-vals)
    (cond ((null? p-vals) #f)
          ((eq? val (vector-ref (car p-vals) 0)) #t)
          (else (radiobutton-legal val (cdr p-vals)))))

  (define (radiobutton-strings p-vals)
    (if (null? p-vals)
        ()
        (cons (vector-ref (car p-vals) 1)
              (cons (vector-ref (car p-vals) 2)
                    (radiobutton-strings (cdr p-vals))))))

  (let* ((value default-value)
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name sort-tag 'radiobutton documentation-string
     (lambda () value)
     (lambda (x)
       (if (radiobutton-legal x ok-values)
           (begin
             (set! value x)
             (if (procedure? setter-function-called-cb)
                 (setter-function-called-cb x)))
           (gnc:error "Illegal Radiobutton option set")))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f (symbol->string value) p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (string? v))
             (set! value (string->symbol v)))))
     (lambda (x)
       (if (radiobutton-legal x ok-values)
           (list #t x)
           (list #f "radiobutton-option: illegal choice")))
     ok-values
     (vector (lambda () (length ok-values))
             (lambda (x) (vector-ref (list-ref ok-values x) 0))
             (lambda (x) (vector-ref (list-ref ok-values x) 1))
             (lambda (x) (vector-ref (list-ref ok-values x) 2))
             (lambda (x)
               (gnc:multichoice-list-lookup ok-values x)))
     (lambda () (radiobutton-strings ok-values)) 
     (and option-widget-changed-cb
          (lambda (x) (option-widget-changed-cb x))))))


;; list options use the option-data in the same way as multichoice
;; options. List options allow the user to select more than one option.
(define (gnc:make-list-option
         section
         name
         sort-tag
         documentation-string
         default-value
         ok-values)

  (define (legal-value? value legal-values)
    (cond ((null? legal-values) #f)
          ((eq? value (vector-ref (car legal-values) 0)) #t)
          (else (legal-value? value (cdr legal-values)))))

  (define (list-legal values)
    (cond ((null? values) #t)
          (else
           (and
            (legal-value? (car values) ok-values)
            (list-legal (cdr values))))))

  (define (list-strings p-vals)
    (if (null? p-vals)
        ()
        (cons (vector-ref (car p-vals) 1)
              (cons (vector-ref (car p-vals) 2)
                    (list-strings (cdr p-vals))))))

  (let* ((value default-value)
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name sort-tag 'list documentation-string
     (lambda () value)
     (lambda (x)
       (if (list-legal x)
           (set! value x)
           (gnc:error "Illegal list option set")))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p)
       (define (save-item list count)
         (if (not (null? list))
             (let ((key (string-append "item" (gnc:value->string count))))
               (kvp-frame-set-slot-path-gslist f (car list) (append p (list key)))
               (save-item (cdr list) (+ 1 count)))))
       (kvp-frame-set-slot-path-gslist f (length value) (append p '("len")))
       (save-item value 0))
     (lambda (f p)
       (let ((len (kvp-frame-get-slot-path-gslist f (append p '("len")))))
         (define (load-item count)
           (if (< count len)
               (let* ((key (string-append "item" (gnc:value->string count)))
                      (val (kvp-frame-get-slot-path-gslist
                            f (append p (list key)))))
                 (cons val (load-item (+ count 1))))
               '()))

         (if (and len (integer? len))
             (set! value (load-item 0)))))
     (lambda (x)
       (if (list-legal x)
           (list #t x)
           (list #f "list-option: illegal value")))
     ok-values     
     (vector (lambda () (length ok-values))
             (lambda (x) (vector-ref (list-ref ok-values x) 0))
             (lambda (x) (vector-ref (list-ref ok-values x) 1))
             (lambda (x) (vector-ref (ref ok-values x) 2))
             (lambda (x) (gnc:multichoice-list-lookup ok-values x)))
     (lambda () (list-strings ok-values)) #f)))

;; number range options use the option-data as a list whose
;; elements are: (lower-bound upper-bound num-decimals step-size)
(define (gnc:make-number-range-option
         section
         name
         sort-tag
         documentation-string
         default-value
         lower-bound
         upper-bound
         num-decimals
         step-size)
  (let* ((value default-value)
         (value->string (lambda () (number->string value))))
    (gnc:make-option
     section name sort-tag 'number-range documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (f p) (kvp-frame-set-slot-path-gslist f (symbol->string value) p))
     (lambda (f p)
       (let ((v (kvp-frame-get-slot-path-gslist f p)))
         (if (and v (number? v))
             (set! value v))))
     (lambda (x)
       (cond ((not (number? x)) (list #f "number-range-option: not a number"))
             ((and (>= value lower-bound)
                   (<= value upper-bound))
              (list #t x))
             (else (list #f "number-range-option: out of range"))))
     (list lower-bound upper-bound num-decimals step-size)
     #f #f #f)))

(define (gnc:make-internal-option
         section
         name
         default-value)
  (let* ((value default-value)
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name "" 'internal #f
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     #f
     #f
     (lambda (x) (list #t x))
     #f #f #f #f)))


(define (gnc:make-query-option
         section
         name
         default-value)
  (let* ((value (if (list? default-value)
                    default-value
                    (gnc-query2scm default-value)))
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name "" 'query #f
     (lambda () value)
     (lambda (x) (set! value (if (list? x) x (gnc-query2scm x))))
     (lambda () (if (list? default-value)
                    default-value
                    (gnc-query2scm default-value)))
     (gnc:restore-form-generator value->string)
     #f
     #f
     (lambda (x) (list #t x))
     #f #f #f #f)))


;; Color options store rgba values in a list.
;; The option-data is a list, whose first element
;; is the range of possible rgba values and whose
;; second element is a boolean indicating whether
;; to use alpha transparency.
(define (gnc:make-color-option
         section
         name
         sort-tag
         documentation-string
         default-value
         range
         use-alpha)

  (define (canonicalize values)
    (map exact->inexact values))

  (define (values-in-range values)
    (if (null? values)
        #t
        (let ((value (car values)))
          (and (number? value)
               (>= value 0)
               (<= value range)
               (values-in-range (cdr values))))))

  (define (validate-color color)
    (cond ((not (list? color)) (list #f "color-option: not a list"))
          ((not (= 4 (length color))) (list #f "color-option: wrong length"))
          ((not (values-in-range color))
           (list #f "color-option: bad color values"))
          (else (list #t color))))

  (let* ((value (canonicalize default-value))
         (value->string (lambda ()
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name sort-tag 'color documentation-string
     (lambda () value)
     (lambda (x) (set! value (canonicalize x)))
     (lambda () (canonicalize default-value))
     (gnc:restore-form-generator value->string)
     #f
     #f
     validate-color
     (list range use-alpha)
     #f #f #f)))

(define (gnc:color->hex-string color range)
  (define (html-value value)
    (inexact->exact
     (min 255.0
          (truncate (* (/ 255.0 range) value)))))
  (define (number->hex-string number)
    (let ((ret (number->string number 16)))
      (cond ((< (string-length ret) 2) (string-append "0" ret))
            (else ret))))
  (let ((red (car color))
        (green (cadr color))
        (blue (caddr color)))
    (string-append
     (number->hex-string (html-value red))
     (number->hex-string (html-value green))
     (number->hex-string (html-value blue)))))

(define (gnc:color->html color range)
  (string-append "#"
                 (gnc:color->hex-string color range)))

(define (gnc:color-option->html color-option)
  (let ((color (gnc:option-value color-option))
        (range (car (gnc:option-data color-option))))
    (gnc:color->html color range)))

(define (gnc:color-option->hex-string color-option)
  (let ((color (gnc:option-value color-option))
        (range (car (gnc:option-data color-option))))
    (gnc:color->hex-string color range)))

;;
;; dateformat option
;;
(define (gnc:make-dateformat-option
         section
         name
         sort-tag
         documentation-string
         default-value)

  (define (def-value)
    (if (list? default-value)
        default-value
        '(locale number #t "")))

  (let* ((value (def-value))
         (value->string (lambda () 
                          (string-append "'" (gnc:value->string value)))))
    (gnc:make-option
     section name sort-tag 'dateformat documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () (def-value))
     (gnc:restore-form-generator value->string)
     (lambda (f p)
       (kvp-frame-set-slot-path-gslist
        f (symbol->string (car value)) (append p '("fmt")))
       (kvp-frame-set-slot-path-gslist
        f (symbol->string (cadr value)) (append p '("month")))
       (kvp-frame-set-slot-path-gslist
        f (if (caddr value) 1 0) (append p '("years")))
       (kvp-frame-set-slot-path-gslist f (cadddr value) (append p '("custom"))))
     (lambda (f p)
       (let ((fmt (kvp-frame-get-slot-path-gslist f (append p '("fmt"))))
             (month (kvp-frame-get-slot-path-gslist f (append p '("month"))))
             (years (kvp-frame-get-slot-path-gslist f (append p '("years"))))
             (custom (kvp-frame-get-slot-path-gslist f (append p '("custom")))))
         (if (and
              fmt (string? fmt)
              month (string? month)
              years (number? years)
              custom (string? custom))
             (set! value (list (string->symbol fmt) (string->symbol month)
                               (if (= years 0) #f #t) custom)))))
     (lambda (x)
       (cond ((not (list? x)) (list #f "dateformat-option: not a list"))
             ((not (= (length x) 4))
              (list #f "dateformat-option: wrong list length" (length x)))
             ((not (symbol? (car x)))
              (list #f "dateformat-option: no format symbol"))
             ((not (symbol? (cadr x)))
              (list #f "dateformat-option: no months symbol"))
             ((not (string? (cadddr x)))
              (list #f "dateformat-option: no custom string"))
             (else (list #t x))))
     #f #f #f #f)))

(define (gnc:dateformat-get-format v)
  (cadddr v))

;; Create a new options database
(define (gnc:new-options)
  (define option-hash (make-hash-table 23))

  (define options-changed #f)
  (define changed-hash (make-hash-table 23))

  (define callback-hash (make-hash-table 23))
  (define last-callback-id 0)

  (define (lookup-option section name)
    (let ((section-hash (hash-ref option-hash section)))
      (if section-hash
          (hash-ref section-hash name)
          #f)))

  (define (option-changed section name)
    (set! options-changed #t)
    (let ((section-changed-hash (hash-ref changed-hash section)))
      (if (not section-changed-hash)
          (begin
            (set! section-changed-hash (make-hash-table 23))
            (hash-set! changed-hash section section-changed-hash)))
      (hash-set! section-changed-hash name #t)))

  (define (clear-changes)
    (set! options-changed #f)
    (set! changed-hash (make-hash-table 23)))

  (define (register-option new-option)
    (let* ((name (gnc:option-name new-option))
           (section (gnc:option-section new-option))
           (section-hash (hash-ref option-hash section)))
      (if (not section-hash)
          (begin
            (set! section-hash (make-hash-table 23))
            (hash-set! option-hash section section-hash)))
      (hash-set! section-hash name new-option)
      (gnc:option-set-changed-callback
       new-option
       (lambda () (option-changed section name)))))

;   Call (thunk option) for each option in the database
  (define (options-for-each thunk)
    (define (section-for-each section-hash thunk)
      (hash-for-each
       (lambda (name option)
         (thunk option))
       section-hash))
    (hash-for-each
     (lambda (section hash)
       (section-for-each hash thunk))
     option-hash))

  (define (options-for-each-general section-thunk option-thunk)
    (define (section-for-each section-hash thunk)
      (hash-for-each
       (lambda (name option)
         (thunk option))
       section-hash))
    (hash-for-each
     (lambda (section hash)
       (if section-thunk
           (section-thunk section hash))
       (if option-thunk
           (section-for-each hash option-thunk)))
     option-hash))

  (define (generate-restore-forms options-string)

    (define (generate-option-restore-form option restore-code)
      (let* ((section (gnc:option-section option))
             (name (gnc:option-name option)))
        (string-append
         "(let ((option (gnc:lookup-option " options-string "\n"
         "                                 " (gnc:value->string section) "\n"
         "                                 " (gnc:value->string name) ")))\n"
         "  (" restore-code " option))\n\n")))

    (define (generate-forms port)
      (options-for-each-general
       (lambda (section hash)
         (display
          (string-append "\n; Section: " section "\n\n")
          port))
       (lambda (option)
         (let ((value (gnc:option-value option))
               (default-value (gnc:option-default-value option)))
           (if (not (equal? value default-value))
            (let* ((generator (gnc:option-generate-restore-form option))
                   (restore-code (false-if-exception (generator))))
              (if restore-code
                  (display
                   (generate-option-restore-form option restore-code)
                   port))))))))

    (call-with-output-string generate-forms))

  (define (scm->kvp kvp-frame key-path)
    (options-for-each
     (lambda (option)
       (let ((value (gnc:option-value option))
             (default-value (gnc:option-default-value option))
             (section (gnc:option-section option))
             (name (gnc:option-name option)))
         (gnc:debug "value: " value "; default: " default-value
                    "; section: " section "; name: " name)
         (if (not (equal? value default-value))
             (let ((save-fcn (gnc:option-scm->kvp option)))
               (gnc:debug "save-fcn: " save-fcn)
               (if save-fcn
                   (save-fcn kvp-frame (append key-path
                                               (list section name))))))))))

  (define (kvp->scm kvp-frame key-path)
    (options-for-each
     (lambda (option)
       (let ((section (gnc:option-section option))
             (name (gnc:option-name option))
             (load-fcn (gnc:option-kvp->scm option)))
         (if load-fcn
             (load-fcn kvp-frame (append key-path
                                         (list section name))))))))

  (define (register-callback section name callback)
    (let ((id last-callback-id)
          (data (list section name callback)))
      (set! last-callback-id (+ last-callback-id 1))
      (hashv-set! callback-hash id data)
      id))

  (define (unregister-callback-id id)
    (if (hashv-ref callback-hash id)
        (hashv-remove! callback-hash id)
        (gnc:error "options:unregister-callback-id: no such id\n")))

  (define (run-callbacks)
    (define (run-callback id cbdata)
      (let ((section  (car cbdata))
            (name     (cadr cbdata))
            (callback (caddr cbdata)))
        (if (not section)
            (callback)
            (let ((section-changed-hash (hash-ref changed-hash section)))
              (if section-changed-hash
                  (if (not name)
                      (callback)
                      (if (hash-ref section-changed-hash name)
                          (callback))))))))

    (if options-changed
        (let ((cblist '()))
          (hash-for-each 
           (lambda (k v) (set! cblist (cons (cons k v) cblist)))
           callback-hash)
          (set! cblist (sort cblist 
                             (lambda (a b) 
                               (< (car a) (car b)))))
          (for-each 
           (lambda (elt) (run-callback (car elt) (cdr elt))) 
           cblist)))
    (clear-changes))
  
  (define default-section #f)

  (define (touch)
    (set! options-changed #t)
    (run-callbacks))
  
  (define (set-default-section section-name)
    (set! default-section section-name))

  (define (get-default-section)
    default-section)

  (define (dispatch key)
    (case key
      ((lookup) lookup-option)
      ((register-option) register-option)
      ((register-callback) register-callback)
      ((unregister-callback-id) unregister-callback-id)
      ((for-each) options-for-each)
      ((for-each-general) options-for-each-general)
      ((generate-restore-forms) generate-restore-forms)
      ((scm->kvp) scm->kvp)
      ((kvp->scm) kvp->scm)
      ((touch) touch)
      ((clear-changes) clear-changes)
      ((run-callbacks) run-callbacks)
      ((set-default-section) set-default-section)
      ((get-default-section) get-default-section)
      (else (gnc:warn "options: bad key: " key "\n"))))

  dispatch)

(define (gnc:register-option options new-option)
  ((options 'register-option) new-option))

(define (gnc:options-register-callback section name callback options)
  ((options 'register-callback) section name callback))

(define (gnc:options-register-c-callback section name c-callback data options)
  (let ((callback (lambda () (gncp-option-invoke-callback c-callback data))))
    ((options 'register-callback) section name callback)))

(define (gnc:options-unregister-callback-id id options)
  ((options 'unregister-callback-id) id))

(define (gnc:options-for-each thunk options)
  ((options 'for-each) thunk))

(define (gnc:options-for-each-general section-thunk option-thunk options)
  ((options 'for-each-general) section-thunk option-thunk))

(define (gnc:lookup-option options section name)
  (if options
      ((options 'lookup) section name)
      #f))

(define (gnc:generate-restore-forms options options-string)
  ((options 'generate-restore-forms) options-string))

(define (gnc:options-scm->kvp options kvp-frame key-path clear-kvp?)
  (if clear-kvp?
      (gnc-kvp-frame-delete-at-path kvp-frame key-path))
  ((options 'scm->kvp) kvp-frame key-path))

(define (gnc:options-kvp->scm options kvp-frame key-path)
  ((options 'kvp->scm) kvp-frame key-path))

(define (gnc:options-clear-changes options)
  ((options 'clear-changes)))

(define (gnc:options-touch options)
  ((options 'touch)))

(define (gnc:options-run-callbacks options)
  ((options 'run-callbacks)))

(define (gnc:options-set-default-section options section-name)
  ((options 'set-default-section) section-name))

(define (gnc:options-get-default-section options)
  ((options 'get-default-section)))

;; Copies all values from src-options to dest-options, that is, it
;; copies the values of all options from src which exist in dest to
;; there.
(define (gnc:options-copy-values src-options dest-options)
  (if 
   dest-options
   (gnc:options-for-each 
    (lambda (src-option) 
      (let ((dest-option (gnc:lookup-option dest-options 
                                            (gnc:option-section src-option)
                                            (gnc:option-name src-option))))
        (if dest-option
            (gnc:option-set-value dest-option 
                                  (gnc:option-value src-option)))))
    src-options)))

(define (gnc:send-options db_handle options)
  (gnc:options-for-each
   (lambda (option)
     (gnc-option-db-register-option db_handle option))
   options))

(define (gnc:save-options options options-string file header truncate?)
  (let ((code (gnc:generate-restore-forms options options-string))
        (port (false-if-exception
               (if truncate? 
                   (open file (logior O_WRONLY O_CREAT O_TRUNC))
                   (open file (logior O_WRONLY O_CREAT O_APPEND))))))
    (if port (begin
               (display header port)
               (display code port)
               (close port)))))

(define (gnc:options-make-end-date! options pagename optname sort-tag info)
  (gnc:register-option 
   options  
   (gnc:make-date-option
    pagename optname 
    sort-tag info
    (lambda () 
      (cons 'relative 'end-accounting-period))
    #f 'both 
    '(
      today 
      end-this-month
      end-prev-month 
      end-current-quarter 
      end-prev-quarter
      end-cal-year 
      end-prev-year
      end-accounting-period
      ))))

(define (gnc:options-make-date-interval! options pagename name-from info-from
                                         name-to info-to sort-tag)
  (gnc:register-option 
   options  
   (gnc:make-date-option
    pagename name-from
    (string-append sort-tag "a") info-from
    (lambda () (cons 'relative 'start-accounting-period))
    #f 'both 
    '(
      today
      start-this-month 
      start-prev-month 
      start-current-quarter
      start-prev-quarter
      start-cal-year 
      start-prev-year
      start-accounting-period
      )))
  (gnc:options-make-end-date! options pagename name-to
                              (string-append sort-tag "b") info-to))
