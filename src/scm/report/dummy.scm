;; -*-scheme-*-

(begin

  (define gnc:*dummy-options* '())

  (define (gnc:register-dummy-option new-option)
    (set! gnc:*dummy-options*
          (gnc:register-option gnc:*dummy-options* new-option))
    new-option)

  (define boolop
    (gnc:register-dummy-option
     (gnc:make-simple-boolean-option
      "Page One" "Boolean Option"
      "a" "This is a boolean option." #t)))

  (define multop
    (gnc:register-dummy-option
     (gnc:make-multichoice-option
      "Page Two" "Multi Choice Option"
      "a" "This is a multi choice option." 'us
      (list #(us "US" "US-style: mm/dd/yyyy")
            #(uk "UK" "UK-style dd/mm/yyyy")
            #(ce "Europe" "Continental Europe: dd.mm.yyyy")
            #(iso "ISO" "ISO Standard: yyyy-mm-dd")
            #(locale "Locale" "Take from system locale")))))

  (define strop
    (gnc:register-dummy-option
     (gnc:make-string-option
      "Page Two" "String Option"
      "b" "This is a string option" "Hello, World.")))

  (define dateop
    (gnc:register-dummy-option
     (gnc:make-date-option
      "Time and Date" "Just a Date Option"
      "a" "This is a date option"
      (lambda () (cons (current-time) 0))
      #f)))

  (define dateop2
    (gnc:register-dummy-option
     (gnc:make-date-option
      "Time and Date" "Time and Date Option"
      "a" "This is a date option with time"
      (lambda () (cons (current-time) 0))
      #t)))

  (define account-list-op
    (gnc:register-dummy-option
     (gnc:make-account-list-option
      "Page One" "An account list option"
      "b" "This is an account list option"
      (lambda () (gnc:get-current-accounts))
      #f #t)))

  (define (op-value op)
    (let ((getter (gnc:option-getter op)))
      (getter)))

  (define (account-list)
    (let* ((accounts (op-value account-list-op))
           (names (map gnc:account-get-name accounts)))
      (list "<ul>"
            (map (lambda (name) (list "<li>" name "</li>")) names)
            "</ul>")))

  (gnc:define-report
   ;; version
   1
   ;; Menu name
   "Dummy"
   ;; Options
   gnc:*dummy-options*
   ;; Rendering thunk. See report.scm for details.
   (lambda (options)
     (let ((time-string (strftime "%c" (localtime (current-time))))
           (date-string (strftime "%x" (localtime (car (op-value dateop)))))
           (date-string2 (strftime "%c" (localtime (car (op-value dateop2))))))
       (list
        "<html>"
        "<body bgcolor=#99ccff>"
        "The current time is " time-string "."
        "<br>"
        "The boolean op is " (if (op-value boolop) "true." "false.")
        "<br>"
        "The multi op is " (symbol->string (op-value multop))
        "<br>"
        "The string op is " (op-value strop)
        "<br>"
        "The date op is " date-string
        "<br>"
        "The date and time op is " date-string2
        "<br>"
        "The accounts are:"
        (account-list)
        "</body>"
        "</html>"))))
  )
