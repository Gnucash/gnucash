;; -*-scheme-*-

;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

;; Putting your functions in a (let ()) block hides them
;; from the rest of guile.
(let ()

  ;; This function will generate a set of options that GnuCash
  ;; will use to display a dialog where the user can select
  ;; values for your report's parameters.
  (define (options-generator)

    ;; This will be the new set of options.
    (define gnc:*hello-world-options* (gnc:new-options))

    ;; This is just a helper function for making options.
    ;; See gnucash/src/scm/options.scm for details.
    (define (gnc:register-hello-world-option new-option)
      (gnc:register-option gnc:*hello-world-options* new-option))

    ;; This is a boolean option. It is in Section 'Hello, World!'
    ;; and is named 'Boolean Option'. Its sorting key is 'a',
    ;; thus it will come before options with sorting keys
    ;; 'b', 'c', etc. in the same section. The default value
    ;; is #t (true). The phrase 'This is a boolean option'
    ;; will be displayed as help text when the user puts
    ;; the mouse pointer over the option.
    (gnc:register-hello-world-option
     (gnc:make-simple-boolean-option
      "Hello, World!" "Boolean Option"
      "a" "This is a boolean option." #t))

    ;; This is a multichoice option. The user can choose between
    ;; the values 'first, 'second, 'third, or 'fourth. These are guile
    ;; symbols. The value 'first will be displayed as "First Option"
    ;; and have a help string of "Help for first option.". The default
    ;; value is 'third.
    (gnc:register-hello-world-option
     (gnc:make-multichoice-option
      "Hello, World!" "Multi Choice Option"
      "b" "This is a multi choice option." 'third
      (list #(first  "First Option"   "Help for first option")
            #(second "Second Option"  "Help for second option")
            #(third  "Third Option"   "Help for third option")
            #(fourth "Fourth Options" "The fourth option rules!"))))

    ;; This is a string option. Users can type anything they want
    ;; as a value. The default value is "Hello, World". This is
    ;; in the same section as the option above. It will be shown
    ;; after the option above because its key is 'b' while the
    ;; other key is 'a'.
    (gnc:register-hello-world-option
     (gnc:make-string-option
      "Hello, World!" "String Option"
      "c" "This is a string option" "Hello, World"))

    ;; This is a date/time option. The user can pick a date and,
    ;; possibly, a time. Times are stored as a pair
    ;; (seconds . nanoseconds) measured from Jan 1, 1970, i.e.,
    ;; Unix time. The last option is false, so the user can only
    ;; select a date, not a time. The default value is the current
    ;; time.
    (gnc:register-hello-world-option
     (gnc:make-date-option
      "Hello, World!" "Just a Date Option"
      "d" "This is a date option"
      (lambda () (cons (current-time) 0))
      #f))

    ;; This is another date option, but the user can also select
    ;; the time.
    (gnc:register-hello-world-option
     (gnc:make-date-option
      "Hello, World!" "Time and Date Option"
      "e" "This is a date option with time"
      (lambda () (cons (current-time) 0))
      #t))

    ;; This is a color option, defined by rgba values. A color value
    ;; is a list where the elements are the red, green, blue, and
    ;; alpha channel values respectively. The penultimate argument
    ;; (255) is the allowed range of rgba values. The final argument
    ;; (#f) indicates the alpha value should be ignored. You can get
    ;; a color string from a color option with gnc:color-option->html,
    ;; which will scale the values appropriately according the range.
    (gnc:register-hello-world-option
     (gnc:make-color-option
      "Hello, World!" "Background Color"
      "f" "This is a color option"
      (list #x99 #xcc #xff 0)
      255
      #f))

    ;; This is an account list option. The user can select one
    ;; or (possibly) more accounts from the list of accounts
    ;; in the current file. Values are scheme handles to actual
    ;; C pointers to accounts. They can be used in conjunction
    ;; with the wrapped C functions in gnucash/src/g-wrap/gnc.gwp.
    ;; The #f value indicates that any account will be accepted.
    ;; Instead of a #f values, you could provide a function that
    ;; accepts a list of account values and returns a pair. If
    ;; the first element is #t, the second element is the list
    ;; of accounts actually accepted. If the first element is
    ;; #f, the accounts are rejected and the second element is
    ;; and error string. The last argument is #t which means
    ;; the user is allowed to select more than one account.
    ;; The default value for this option is the currently
    ;; selected account in the main window, if any.
    (gnc:register-hello-world-option
     (gnc:make-account-list-option
      "Hello Again" "An account list option"
      "g" "This is an account list option"
      (lambda () (gnc:get-current-accounts))
      #f #t))

    ;; This is a list option. The user can select one or (possibly)
    ;; more values from a list. The list of acceptable values is
    ;; the same format as a multichoice option. The value of the
    ;; option is a list of symbols.
    (gnc:register-hello-world-option
     (gnc:make-list-option
      "Hello Again" "A list option"
      "h" "This is a list option"
      (list 'good)
      (list #(good  "The Good" "Good option")
            #(bad   "The Bad"  "Bad option")
            #(ugly  "The Ugly" "Ugly option"))))

    ;; This option is for testing. When true, the report generates
    ;; an exception.
    (gnc:register-hello-world-option
     (gnc:make-simple-boolean-option
      "Testing" "Crash the report"
      "a" (string-append "This is for testing. "
                         "Your reports probably shouldn't have an "
                         "option like this.") #f))

    gnc:*hello-world-options*)

  ;; This is a helper function to generate an html list of account names
  ;; given an account list option.
  (define (account-list account-list-op)
    (let* ((accounts (gnc:option-value account-list-op))
           (names (map gnc:account-get-name accounts)))
      (if (null? accounts)
          "<p>There are no selected accounts in the account list option.</p>"
          (list "<p>"
                "The accounts selected in the account list option are:"
                "</p>"
                "<ul>"
                (map (lambda (name) (list "<li>" name "</li>")) names)
                "</ul>"))))

  ;; This is a helper function to generate an html list for the list option.
  (define (list-option-list list-op)
    (let* ((values (gnc:option-value list-op))
           (names (map symbol->string values)))
      (if (null? values)
          "<p>You have selected no values in the list option.</p>"
          (list "<p>"
                "The items selected in the list option are:"
                "</p>"
                "<ul>"
                (map (lambda (name) (list "<li>" name "</li>")) names)
                "</ul>"))))

  ;; Here's another helper function. You can guess what it does.
  (define (bold string)
    (string-append "<b>" string "</b>"))

  ;; Here we define the actual report with gnc:define-report
  (gnc:define-report
   ;; The version of this report.
   1
   ;; The name of this report. This will be used, among other things,
   ;; for making its menu item in the main menu.
   "Hello, World"
   ;; The options generator function defined above.
   options-generator
   ;; This is the rendering function. It accepts a group of options
   ;; and generates a (possibly nested) list of html. The "flattened"
   ;; and concatenated list should be valid html.
   (lambda (options)
     ;; The first thing we do is make local variables for all the specific
     ;; options in the set of options given to the function. This set will
     ;; be generated by the options generator above.
     ;; Use (gnc:lookup-option options section name) to get the option. 
     (let ((boolop (gnc:lookup-option options
                                      "Hello, World!" "Boolean Option"))
           (multop (gnc:lookup-option options
                                      "Hello, World!" "Multi Choice Option"))
           (strop (gnc:lookup-option options
                                     "Hello, World!" "String Option"))
           (dateop (gnc:lookup-option options
                                      "Hello, World!" "Just a Date Option"))
           (dateop2 (gnc:lookup-option options
                                       "Hello, World!" "Time and Date Option"))
           (colorop (gnc:lookup-option options
                                       "Hello, World!" "Background Color"))
           (account-list-op (gnc:lookup-option options
                                               "Hello Again"
                                               "An account list option"))
           (list-op (gnc:lookup-option options
                                       "Hello Again" "A list option"))
           (crash-op (gnc:lookup-option options "Testing" "Crash the report")))

       (let ((time-string (strftime "%X" (localtime (current-time))))
             (date-string (strftime "%x" (localtime (car (gnc:option-value
                                                          dateop)))))
             (date-string2 (strftime "%x %X"
                                     (localtime (car (gnc:option-value
                                                      dateop2))))))

         ;; Crash if asked to.
         (if (gnc:option-value crash-op)
             (string-length #f)) ;; this will crash

         ;; Here's where we generate the html. A real report would need
         ;; much more code and involve many more utility functions. See
         ;; the other reports for details. Note that you can used nested
         ;; lists here, as well as arbitrary functions.
         (list
          "<html>"
          "<body bgcolor=" (gnc:color-option->html colorop) ">"

          "<h2>Hello, World</h2>"

          (list "<p>"
                "This is a sample GnuCash report. "
                "See the guile (scheme) source code in "
                "<tt>" gnc:_share-dir-default_
                "/gnucash/scm/report" "</tt>"
                " for details on writing your own reports, or extending "
                " existing reports."
                "</p>")

          (list "<p>"
                "For help on writing reports, or to contribute your brand "
                "new, totally cool report, consult the mailing list "
                "<a href=\"mailto:gnucash-devel@gnucash.org\">"
                "<tt>gnucash-devel@gnucash.org</tt>" "</a>" ". "
                "For details on subscribing to that list, see "
                "<a href=\"http://www.gnucash.org\">"
                "<tt>www.gnucash.org</tt>" "</a>" "."
                "</p>")

          (list "<p>"
                "The current time is " (bold time-string) "."
                "</p>")

          (list "<p>"
                "The boolean option is "
                (bold (if (gnc:option-value boolop) "true" "false")) "."
                "</p>")

          (list "<p>"
                "The multi-choice option is "
                (bold (symbol->string (gnc:option-value multop))) "."
                "</p>")

          (list "<p>"
                "The string option is " (bold (gnc:option-value strop)) "."
                "</p>")

          (list "<p>"
                "The date option is " (bold date-string) "."
                "</p>")

          (list "<p>"
                "The date and time option is " (bold date-string2) "."
                "</p>")

          (list-option-list list-op)

          (account-list account-list-op)

          (list "<p>"
                "Have a nice day!"
                "</p>")

          "</body>"
          "</html>")))))
  )
