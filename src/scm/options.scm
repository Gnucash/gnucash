;; Scheme code for supporting options

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
	 ;; Validation func should accept a value and return (#t value)
	 ;; on success, and (#f "failure-message") on failure. If #t,
	 ;; the supplied value will be used by the gui to set the option.
         value-validator
	 option-data)
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
            value-validator
            option-data
            (lambda (callback) (set! changed-callback callback)))))

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
(define (gnc:option-value-validator option)
  (vector-ref option 9))
(define (gnc:option-data option)
  (vector-ref option 10))
(define (gnc:option-set-changed-callback option callback)
  (let ((cb-setter (vector-ref option 11)))
    (cb-setter callback)))

(define (gnc:option-value option)
  (let ((getter (gnc:option-getter option)))
    (getter)))

(define (gnc:restore-form-generator value->string)
  (lambda () (string-append
              "(lambda (option) "
              "(if option ((gnc:option-setter option) "
              (value->string)
              ")))")))

(define (gnc:make-string-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-value)
  (let* ((value default-value)
         (value->string (lambda () (string-append "\"" value "\""))))
    (gnc:make-option
     section name sort-tag 'string documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (x)
       (cond ((string? x)(list #t x))
             (else (list #f "string-option: not a string"))))
     #f )))

(define (gnc:make-simple-boolean-option
	 section
	 name
	 sort-tag
	 documentation-string
	 default-value)
  (let* ((value default-value)
         (value->string (lambda () (if value "#t" "#f"))))
    (gnc:make-option
     section name sort-tag 'boolean documentation-string
     (lambda () value)
     (lambda (x) (set! value x))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (x)
       (if (boolean? x)
           (list #t x)
           (list #f "boolean-option: not a boolean")))
     #f )))

;; date options use the option-data as a boolean value. If true,
;; the gui should allow the time to be entered as well.
(define (gnc:make-date-option
         section
         name
         sort-tag
         documentation-string
         default-getter
         show-time)

  (define (date-legal date)
    (and (pair? date) (exact? (car date)) (exact? (cdr date))))

  (let* ((value (default-getter))
         (value->string
          (lambda ()
            (string-append "(" (number->string (car value))
                           " . " (number->string (cdr value)) ")"))))
    (gnc:make-option
     section name sort-tag 'date documentation-string
     (lambda () value)
     (lambda (date)
       (if (date-legal date)
           (set! value date)
           (gnc:error "Illegal date value set")))
     default-getter
     (gnc:restore-form-generator value->string)
     (lambda (date)
       (if (date-legal date)
           (list #t date)
           (list #f "date-option: illegal date")))
     show-time )))

;; account-list options use the option-data as a boolean value.  If
;; true, the gui should allow the user to select multiple accounts.
;; values are always a list of accounts.
(define (gnc:make-account-list-option
         section
         name
         sort-tag
         documentation-string
         default-getter
         value-validator
         multiple-selection)
  (let ((option (default-getter))
        (option-set #f)
        (validator
         (if (not value-validator)
             (lambda (account-list) (list #t account-list))
             value-validator)))
    (gnc:make-option
     section name sort-tag 'account-list documentation-string
     (lambda () (if option-set option (default-getter)))
     (lambda (account-list)
       (let* ((result (validator account-list))
              (valid (car result))
              (value (cadr result)))
         (if valid
             (begin
               (set! option value)
               (set! option-set #t))
             (gnc:error "Illegal account list value set"))))
     default-getter
     #f
     validator
     multiple-selection )))

;; multichoice options use the option-data as a list of vectors.
;; Each vector contains a permissible value (scheme symbol) and
;; a description string.
(define (gnc:make-multichoice-option
         section
         name
         sort-tag
         documentation-string
         default-value
         ok-values)

  (define (multichoice-legal val p-vals)
    (cond ((null? p-vals) #f)
          ((eq? val (vector-ref (car p-vals) 0)) #t)
          (else multichoice-legal (cdr p-vals))))

  (let* ((value default-value)
         (value->string (lambda ()
                          (string-append "'" (symbol->string value)))))
    (gnc:make-option
     section name sort-tag 'multichoice documentation-string
     (lambda () value)
     (lambda (x)
       (if (multichoice-legal x ok-values)
           (set! value x)
           (gnc:error "Illegal Multichoice option set")))
     (lambda () default-value)
     (gnc:restore-form-generator value->string)
     (lambda (x)
       (if (multichoice-legal x ok-values)
           (list #t x)
           (list #f "multichoice-option: illegal choice")))
     ok-values)))


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

  ; Call (thunk option) for each option in the database
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
       (section-thunk section hash)
       (if option-thunk
           (section-for-each hash option-thunk)))
     option-hash))

  (define (generate-restore-forms options-string)

    (define (generate-option-restore-form option restore-code)
      (let* ((section (gnc:option-section option))
             (name (gnc:option-name option)))
        (string-append
         "(let ((option (gnc:lookup-option " options-string "\n"
         "                                 \"" section "\"\n"
         "                                 \"" name "\")))\n"
         "  (" restore-code " option))\n\n")))

    (define (generate-forms port)
      (options-for-each-general
       (lambda (section hash)
         (display
          (string-append "\n; Section: " section "\n\n")
          port))
       (lambda (option)
         (let* ((generator (gnc:option-generate-restore-form option))
                (restore-code (false-if-exception (generator))))
           (if restore-code
               (display
                (generate-option-restore-form option restore-code)
                port))))))

    (let ((header "; GnuCash Configuration Options\n\n")
          (forms (call-with-output-string generate-forms)))
      (string-append header forms)))

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
        (hash-for-each run-callback callback-hash))
    (clear-changes))

  (define (dispatch key)
    (cond ((eq? key 'lookup) lookup-option)
          ((eq? key 'register-option) register-option)
          ((eq? key 'register-callback) register-callback)
          ((eq? key 'unregister-callback-id) unregister-callback-id)
          ((eq? key 'for-each) options-for-each)
          ((eq? key 'for-each-general) options-for-each-general)
          ((eq? key 'generate-restore-forms) generate-restore-forms)
          ((eq? key 'clear-changes) clear-changes)
          ((eq? key 'run-callbacks) run-callbacks)
          (else (gnc:warn "options: bad key: " key "\n"))))

  dispatch)

(define (gnc:register-option options new-option)
  ((options 'register-option) new-option))

(define (gnc:options-register-callback section name callback options)
  ((options 'register-callback) section name callback))

(define (gnc:options-register-c-callback section name c-callback data options)
  (let ((callback (lambda () (gnc:option-invoke-callback c-callback data))))
    ((options 'register-callback) section name callback)))

(define (gnc:options-unregister-callback-id id options)
  ((options 'unregister-callback-id) id))

(define (gnc:options-for-each thunk options)
  ((options 'for-each) thunk))

(define (gnc:options-for-each-general section-thunk option-thunk options)
  ((options 'for-each-general) section-thunk option-thunk))

(define (gnc:lookup-option options section name)
  ((options 'lookup) section name))

(define (gnc:generate-restore-forms options options-string)
  ((options 'generate-restore-forms) options-string))

(define (gnc:options-clear-changes options)
  ((options 'clear-changes)))

(define (gnc:options-run-callbacks options)
  ((options 'run-callbacks)))

(define (gnc:send-options db_handle options)
  (gnc:options-for-each
   (lambda (option)
     (gnc:option-db-register-option db_handle option))
   options))

(define (gnc:save-options options options-string file)
  (let ((code (gnc:generate-restore-forms options options-string))
        (port (open file (logior O_WRONLY O_CREAT O_TRUNC))))
    (if port (begin
               (display code port)
               (close port)))))
