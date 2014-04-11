;; -*-scheme-*-

;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

(define-module (gnucash report hello-world))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(debug-enable 'debug)
(debug-enable 'backtrace)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for your report's parameters.
(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         ;; This is just a helper function for making options.
         ;; See gnucash/src/app-utils/options.scm for details.
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))
    
    ;; This is a boolean option. It is in Section 'Hello, World!'
    ;; and is named 'Boolean Option'. Its sorting key is 'a',
    ;; thus it will come before options with sorting keys
    ;; 'b', 'c', etc. in the same section. The default value
    ;; is #t (true). The phrase 'This is a boolean option'
    ;; will be displayed as help text when the user puts
    ;; the mouse pointer over the option.
    (add-option
     (gnc:make-simple-boolean-option
      (N_ "Hello, World!") (N_ "Boolean Option")
      "a" (N_ "This is a boolean option.") #t))
    
    ;; This is a multichoice option. The user can choose between
    ;; the values 'first, 'second, 'third, or 'fourth. These are guile
    ;; symbols. The value 'first will be displayed as "First Option"
    ;; and have a help string of "Help for first option.". The default
    ;; value is 'third.
    (add-option
     (gnc:make-multichoice-option
      (N_ "Hello, World!") (N_ "Multi Choice Option")
      "b" (N_ "This is a multi choice option.") 'third
      (list (list->vector
             (list 'first
                   (N_ "First Option")
                   (N_ "Help for first option")))
            (list->vector
             (list 'second
                   (N_ "Second Option")
                   (N_ "Help for second option")))
            (list->vector
             (list 'third
                   (N_ "Third Option")
                   (N_ "Help for third option")))
            (list->vector
             (list 'fourth
                   (N_ "Fourth Options")
                   (N_ "The fourth option rules!"))))))
    
    ;; This is a string option. Users can type anything they want
    ;; as a value. The default value is "Hello, World". This is
    ;; in the same section as the option above. It will be shown
    ;; after the option above because its key is 'b' while the
    ;; other key is 'a'.
    (add-option
     (gnc:make-string-option
      (N_ "Hello, World!") (N_ "String Option")
      "c" (N_ "This is a string option") (N_ "Hello, World")))
    
    ;; This is a date/time option. The user can pick a date and,
    ;; possibly, a time. Times are stored as a pair
    ;; (seconds . nanoseconds) measured from Jan 1, 1970, i.e.,
    ;; Unix time. The last option is false, so the user can only
    ;; select a date, not a time. The default value is the current
    ;; time.
    (add-option
     (gnc:make-date-option
      (N_ "Hello, World!") (N_ "Just a Date Option")
      "d" (N_ "This is a date option")
      (lambda () (cons 'absolute (cons (current-time) 0)))
      #f 'absolute #f ))
    
    ;; This is another date option, but the user can also select
    ;; the time.
    (add-option
     (gnc:make-date-option
      (N_ "Hello, World!") (N_ "Time and Date Option")
      "e" (N_ "This is a date option with time")
      (lambda () (cons 'absolute (cons (current-time) 0)))
      #t 'absolute #f ))
    
    (add-option
     (gnc:make-date-option
      (N_ "Hello, World!") (N_ "Combo Date Option")
      "y" (N_ "This is a combination date option")
      (lambda () (cons 'relative 'start-cal-year))
      #f 'both '(start-cal-year start-prev-year end-prev-year) ))
    
    (add-option
     (gnc:make-date-option
      (N_ "Hello, World!") (N_ "Relative Date Option")
      "x" (N_ "This is a relative date option")
      (lambda () (cons 'relative 'start-cal-year))
      #f 'relative '(start-cal-year start-prev-year end-prev-year) ))
    
    ;; This is a number range option. The user can enter a number
    ;; between a lower and upper bound given below. There are also
    ;; arrows the user can click to go up or down, the amount changed
    ;; by a single click is given by the step size.
    (add-option
     (gnc:make-number-range-option
      (N_ "Hello, World!") (N_ "Number Option")
      "ee" (N_ "This is a number option.")
      1500.0  ;; default
      0.0     ;; lower bound
      10000.0 ;; upper bound
      2.0     ;; number of decimals
      0.01    ;; step size
      ))
    
    ;; This is a color option, defined by rgba values. A color value
    ;; is a list where the elements are the red, green, blue, and
    ;; alpha channel values respectively. The penultimate argument
    ;; (255) is the allowed range of rgba values. The final argument
    ;; (#f) indicates the alpha value should be ignored. You can get
    ;; a color string from a color option with gnc:color-option->html,
    ;; which will scale the values appropriately according the range.
    (add-option
     (gnc:make-color-option
      (N_ "Hello, World!") (N_ "Background Color")
      "f" (N_ "This is a color option")
      (list #xf6 #xff #xdb 0)
      255
      #f))
    (add-option
     (gnc:make-color-option
      (N_ "Hello, World!") (N_ "Text Color")
      "f" (N_ "This is a color option")
      (list #x00 #x00 #x00 0)
      255
      #f))
    
    ;; This is an account list option. The user can select one
    ;; or (possibly) more accounts from the list of accounts
    ;; in the current file. Values are scheme handles to actual
    ;; C pointers to accounts. 
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
    (add-option
     (gnc:make-account-list-option
      (N_ "Hello Again") (N_ "An account list option")
      "g" (N_ "This is an account list option")
      ;; FIXME : this used to be gnc:get-current-accounts, but 
      ;; that doesn't exist any more.
      (lambda () '())
      #f #t))
    
    ;; This is a list option. The user can select one or (possibly)
    ;; more values from a list. The list of acceptable values is
    ;; the same format as a multichoice option. The value of the
    ;; option is a list of symbols.
    (add-option
     (gnc:make-list-option
      (N_ "Hello Again") (N_ "A list option")
      "h" (N_ "This is a list option")
      (list 'good)
      (list (list->vector
             (list 'good
                   (N_ "The Good")
                   (N_ "Good option")))
            (list->vector
             (list 'bad
                   (N_ "The Bad")
                   (N_ "Bad option")))
            (list->vector
             (list 'ugly
                   (N_ "The Ugly")
                   (N_ "Ugly option"))))))
    
    ;; This option is for testing. When true, the report generates
    ;; an exception.
    (add-option
     (gnc:make-simple-boolean-option
      (N_ "Testing") (N_ "Crash the report")
      "a" 
      (N_ "This is for testing. \
Your reports probably shouldn't have an \
option like this.") 
      #f))
    
    (gnc:options-set-default-section options "Hello, World!")      
    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (hello-world-renderer report-obj)
  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((bool-val     (op-value "Hello, World!" "Boolean Option"))
        (mult-val     (op-value "Hello, World!" "Multi Choice Option"))
        (string-val   (op-value "Hello, World!" "String Option"))
        (date-val     (gnc:date-option-absolute-time
                       (op-value "Hello, World!" "Just a Date Option")))
        (date2-val    (gnc:date-option-absolute-time
                       (op-value "Hello, World!" "Time and Date Option")))
        (rel-date-val (gnc:date-option-absolute-time
                       (op-value "Hello, World!" "Relative Date Option")))
        (combo-date-val (gnc:date-option-absolute-time
                         (op-value "Hello, World!" "Combo Date Option")))
        (num-val      (op-value "Hello, World!" "Number Option"))
        (bg-color-op  (get-op   "Hello, World!" "Background Color"))
        (txt-color-op (get-op   "Hello, World!" "Text Color"))
        (accounts     (op-value "Hello Again"   "An account list option"))
        (list-val     (op-value "Hello Again"   "A list option"))
        (crash-val    (op-value "Testing"       "Crash the report"))
        
        ;; document will be the HTML document that we return.
        (document (gnc:make-html-document)))

    ;; Crash if asked to.
    (if crash-val (string-length #f)) ;; string-length needs a string

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; gnc-print-date
    (let ((time-string (strftime "%X" (localtime (current-time))))
          (date-string (strftime "%x" (localtime (car date-val))))
          (date-string2 (strftime "%x %X" (localtime (car date2-val))))
          (rel-date-string (strftime "%x" (localtime (car rel-date-val))))
          (combo-date-string
           (strftime "%x" (localtime (car combo-date-val)))))

      ;; Here's where we fill the report document with content.  We
      ;; do this by adding 'html objects' such as text, tables, and
      ;; graphs to the html document we already created.
      
      ;; the report's style sheet (an "invisible" option that every
      ;; report has) will usually set the default background color,
      ;; but we can override that here.  You set background color in
      ;; HTML by specifying the "bgcolor" attribute for the <body>
      ;; tag.

      ;; every HTML object has "styles" for markup and data.  the
      ;; style for an HTML tag such as "body" tells the HTML
      ;; document how to render the markup and content for tagged
      ;; elements.  For each tag, you can specify a font-face,
      ;; font-color, and font-size to render the contents of the
      ;; element, and any number of attributes to put in the
      ;; start-tag.  You can pass 'inheritable? #f if you wish the
      ;; style to apply only to markup in the object itself and not
      ;; to its components.  You can also override the tag itself if
      ;; you want to create your own custom markup (see
      ;; documentation).
      
      ;; in this case, we are saying "every time you see <body>
      ;; markup anywhere in 'document' or its components, add the
      ;; attribute "bgcolor=0xXXXXXX" in the start tag, and enclose
      ;; the content in a <font> block to set the font color".
      ;; Altogether, we get
      ;;
      ;; <body bgcolor=0xXXXXXXX>
      ;; <font color="0xXXXXXX"> (body) </font>
      ;; </body>

      ;; of course if a component object explicitly selects a 
      ;; different font that will override the body font.
      
      (gnc:html-document-set-style!
       document "body" 
       'attribute (list "bgcolor" (gnc:color-option->html bg-color-op))
       'font-color (gnc:color-option->html txt-color-op))
      
      ;; the title of the report will be rendered by the 
      ;; selected style sheet.  All we have to do is set it in the
      ;; HTML document.
      
      ;; Note we invoke the _ function upon this string.
      ;; The _ function works the same way as in C -- if a
      ;; translation of the given string is available for the
      ;; current locale, then the translation is returned,
      ;; otherwise the original string is returned.
      (gnc:html-document-set-title! document (_ "Hello, World"))

      ;; we make a "text object" to add a bunch of text to.
      ;; the function gnc:make-html-text can take any number of 
      ;; arguments.  The gnc:html-markup functions are designed
      ;; to work with the style system so that you can control
      ;; the appearance of the report from the Gnucash UI; you 
      ;; should use the HTML markup functions whenever possible
      ;; rather than including literal HTML in your report.

      (gnc:html-document-add-object!
       document
       (gnc:make-html-text         
        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "This is a sample GnuCash report. \
See the guile (scheme) source code in the scm/report directory \
for details on writing your own reports, \
or extending existing reports.")))
        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "For help on writing reports, or to contribute your brand \
new, totally cool report, consult the mailing list %s.")
          (gnc:html-markup-anchor 
           "mailto:gnucash-devel@gnucash.org"
           (gnc:html-markup-tt "gnucash-devel@gnucash.org")))
         (_ "For details on subscribing to that list, see &lt;http://www.gnucash.org/&gt;.")
         (_ "You can learn more about writing scheme at &lt;http://www.scheme.com/tspl2d/&gt;."))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The current time is %s.") 
          (gnc:html-markup-b time-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The boolean option is %s.")
          (gnc:html-markup-b (if bool-val (_ "true") (_ "false")))))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The multi-choice option is %s.")
          (gnc:html-markup-b (symbol->string mult-val))))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The string option is %s.") 
          (gnc:html-markup-b string-val)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The date option is %s.") 
          (gnc:html-markup-b date-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The date and time option is %s.") 
          (gnc:html-markup-b date-string2)))

        (gnc:html-markup-p
         (gnc:html-markup/format 
          (_ "The relative date option is %s.")
          (gnc:html-markup-b rel-date-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format 
          (_ "The combination date option is %s.")
          (gnc:html-markup-b combo-date-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format 
          (_ "The number option is %s.")
          (gnc:html-markup-b (number->string num-val))))

        ;; Here we print the value of the number option formatted as
        ;; currency. When printing currency values, you should use
        ;; the function (xaccPrintAmount), which is defined in
        ;; report-utilities. This functions will format the number
        ;; appropriately in the current locale. Don't try to format
        ;; it yourself -- it will be wrong in other locales.
        (gnc:html-markup-p 
         (gnc:html-markup/format
          (_ "The number option formatted as currency is %s.")
          (gnc:html-markup-b
           (xaccPrintAmount
            (gnc:make-gnc-numeric (inexact->exact num-val) 1)
            (gnc-default-print-info #f)))))))

      ;; you can add as many objects as you want.  Here's another 
      ;; one.  We'll make a single-column table of the selected list 
      ;; options just for grins. 
      (gnc:html-document-add-object!
       document
       (gnc:make-html-text
        (gnc:html-markup-p (_ "Items you selected:"))))

      (if (not (null? list-val))
          (let ((table (gnc:make-html-table)))
            (gnc:html-table-append-column! 
             table (map symbol->string list-val))
            (gnc:html-table-set-caption! table 
                                         (_ "List items selected"))
            (gnc:html-document-add-object! document table))
          (let ((txt (gnc:make-html-text)))
            (gnc:html-text-append!
             txt
             (gnc:html-markup-p (_ "(You selected no list items.)")))
            (gnc:html-document-add-object! document txt)))
      
      ;; here's a bullet list of accounts.  We can mark up the
      ;; account name with an <a></a> anchor with a special HREF to
      ;; open a Gnucash register when the link is clicked.  What you
      ;; need to do is pass the HREF "gnc-register:account=My
      ;; Account Name" to html-markup-anchor.  The account name
      ;; passed must be the "full" account name that you get from
      ;; gnc-account-get-full-name.  You should build this url using
      ;; (gnc-build-url ...)
      ;;
      ;; html-markup-anchor takes the link to jump to as its first
      ;; arg and then puts the remaining args in the body of the
      ;; link).
      ;;
      ;; html-markup-ul makes a "<ul>" unnumbered list, and takes as
      ;; its one argument a list of items to put in <li> blocks.
      (if (not (null? accounts))
          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-ul
             (map 
              (lambda (acct)
                (gnc:html-markup-anchor 
		 (gnc-build-url URL-TYPE-REGISTER
				     (string-append "account=" 
						    (gnc-account-get-full-name
						     acct))
				     "")
                 (xaccAccountGetName acct)))
              accounts))))
          (gnc:html-document-add-object!
           document
           (gnc:make-html-text
            (gnc:html-markup-p (_ "You have selected no accounts.")))))

      (gnc:html-document-add-object! 
       document 
       (gnc:make-html-text 
        (gnc:html-markup-p (_ "Have a nice day!"))))
      
      document)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Hello, World")

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "898d78ec92854402bf76e20a36d24ade"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name (N_ "Sample Report with Examples")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "A sample report with examples.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-utility)

 ;; The options generator function defined above.
 'options-generator options-generator
 
 ;; The rendering function defined above.
 'renderer hello-world-renderer)
