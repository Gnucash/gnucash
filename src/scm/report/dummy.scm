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

  (define (op-value op)
    (let ((getter (gnc:option-getter op)))
      (getter)))

  (gnc:define-report
   ;; version
   1
   ;; Menu name
   "Dummy"
   ;; Options
   gnc:*dummy-options*
   ;; Rendering thunk. See report.scm for details.
   (lambda (options)
     (list
      "<html>"
      "<body bgcolor=#99ccff>"
      "The current time is " (strftime "%c" (localtime (current-time))) "."
      "<br>"
      "The boolean op is " (if (op-value boolop) "true." "false.")
      "<br>"
      "The multi op is " (symbol->string (op-value multop))
      "<br>"
      "The string op is " (op-value strop)
      "</body>"
      "</html>")))
  )
