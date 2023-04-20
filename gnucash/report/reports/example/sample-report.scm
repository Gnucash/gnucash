;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

;; ------------------------------------------------------------------
;; Top-level definitions
;; ------------------------------------------------------------------
(define-module (gnucash reports example sample-report))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))

(debug-enable 'backtrace)

;; initialise values
(define optname-report-title (N_ "Report Title"))
(define opthelp-report-title (N_ "Title for this report."))

;; ------------------------------------------------------------------
;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for your report's parameters.
;; ------------------------------------------------------------------
(define (options-generator)
  (let* ((options (gnc:new-options))
         (optiondb (options #t))) ;; Hack to get the optiondb from options

    ;; This is a boolean option. It is in Section "Tab B"
    ;; and is named 'Boolean Option'. Its sorting key is 'a',
    ;; thus it will come before options with sorting keys
    ;; 'b', 'c', etc. in the same section. The default value
    ;; is #t (true). The phrase 'This is a boolean option'
    ;; will be displayed as help text when the user puts
    ;; the mouse pointer over the option.
    (gnc-register-simple-boolean-option optiondb
     (N_ "Tab B") (N_ "Boolean Option")
     "a" (N_ "This is a boolean option.") #t)

    ;; This is a multichoice option. The user can choose between the
    ;; values 'first, 'second, 'third, or 'fourth. These are guile
    ;; symbols. The value 'first will be displayed as "First Option".
    ;; The default value is 'third.  Note that multichoice option is a
    ;; special case where we need an intermediate scheme function to
    ;; interpret the default value--'third in this case--because it
    ;; can be either a symbol or a number.
    (gnc-register-multichoice-option optiondb
      (N_ "Tab B") (N_ "Multi Choice Option")
      "b" (N_ "This is a multi choice option.") "third"
      (list (vector 'first (N_ "First Option"))
            (vector 'second (N_ "Second Option"))
            (vector 'third (N_ "Third Option"))
            (vector 'fourth (N_ "Fourth Options"))))

    ;; This is a string option. Users can type anything they want
    ;; as a value. The default value is "String Option Default". This is
    ;; in the same section as the option above. It will be shown
    ;; after the option above because its key is 'b' while the
    ;; other key is 'a'.
    (gnc-register-string-option optiondb
      (N_ "Tab B") (N_ "String Option")
      "c" (N_ "This is a string option.") (N_ "String Option Default"))

    ;; The following are date options. There are three here reflecting
    ;; the three types of date controls that can be displayed in the
    ;; options dialog: Absolute, Relative, or Both. You'll usually
    ;; want to use Both, which is the middle example. Other than the
    ;; usual strings the two parameters are a list of relative date
    ;; types and a boolean to indicate whether you want a Both date
    ;; control. Note that to get an absolute control you pass a
    ;; one-item list containing 'absolute and #f. If you pass (list
    ;; 'absolute) #t you'll get a Both but the relative listbox will
    ;; be empty. That will irritate users so avoid doing that.
    (gnc-register-date-option-set optiondb
      (N_ "Tab B") (N_ "Just a Date Option")
      "d" (N_ "This is a date option.")
      (list 'absolute) #f )

    (gnc-register-date-option-set optiondb
      (N_ "Tab B") (N_ "Combo Date Option")
      "y" (N_ "This is a combination date option.")
      '(start-cal-year start-prev-year end-prev-year) #t)

    (gnc-register-date-option-set optiondb
      (N_ "Tab B") (N_ "Relative Date Option")
      "x" (N_ "This is a relative date option.")
      '(start-cal-year start-prev-year end-prev-year) #f)

    ;; This is a number range option. The user can enter a number
    ;; between a lower and upper bound given below. There are also
    ;; arrows or + and - buttons depending on the icon theme that the
    ;; user can click to go up or down, the amount changed by a single
    ;; click is given by the step size.
    (gnc-register-number-range-option optiondb
      (N_ "Tab B") (N_ "Number Option")
      "ee" (N_ "This is a number option.")
      1500.0  ;; default
      0.0     ;; lower bound
      10000.0 ;; upper bound
      0.01    ;; step size
      )

    ;; This is a color option, defined by rgb values. A color value is
    ;; a string representing a 3-byte hex number with the bytes
    ;; representing red, blue, and green values.
    (gnc-register-color-option optiondb
      (N_ "Tab B") (N_ "Background Color")
      "f" (N_ "This is a color option.")
      "f6ffdb")

    ;; This is an account list option. The user can select one or more
    ;; accounts from the list of accounts in the current file. Values
    ;; are GUIDs of the selected accounts. Since those depend on the
    ;; book in use you'll probably want to create a list from the
    ;; account types as we've done here.  There's another function
    ;; gnc-register-account-list-limited-option which takes as a
    ;; second argument a list of account types; only accounts of the
    ;; types in the list, similar to the list passed to
    ;; gnc-account-list-from-types in this example, will be available
    ;; for selection.

    (gnc-register-account-list-option optiondb
      (N_ "Tab A") (N_ "An account list option")
      "g" (N_ "This is an account list option.")
      (gnc-account-list-from-types
       (gnc-get-current-book)
       (list ACCT-TYPE-ASSET ACCT-TYPE-EQUITY ACCT-TYPE-LIABILITY)))

    ;; This is a list option. The user can select one or (possibly)
    ;; more values from a list. The list of acceptable values is
    ;; the same format as a multichoice option. The value of the
    ;; option is a list of symbols.
    (gnc-register-list-option optiondb
      (N_ "Tab A") (N_ "A list option")
      "h" (N_ "This is a list option.")
      (symbol->string 'good)
      (list (vector 'good (N_ "The Good"))
            (vector 'bad (N_ "The Bad"))
            (vector 'ugly (N_ "The Ugly"))))

    ;; This is a Report Title option using constants defined at the
    ;; beginning of the file.
    (gnc-register-string-option optiondb
      (N_ "Testing") optname-report-title
      "c" opthelp-report-title (N_ "Report Title Default"))

    ;; Setting a default section is optional but set in most reports.
    ;; If not set, the default section will be the first section.
    (gnc:options-set-default-section options "Tab B")      
    (GncOptionDBPtr-set-default-section optiondb "Tab B")
 ;; We still need to return the function wrapper instead of the GncOptionDBPtr for all of the options functions in the reports system.
    options))

;; ------------------------------------------------------------------
;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
;; ------------------------------------------------------------------
(define (sample-report-renderer report-obj)
  ;; Helper function for looking up option values.
  (define (op-value section name)
    (gnc-optiondb-lookup-value ((gnc:report-options report-obj) 'lookup)
                                section name))

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((bool-val     (op-value "Tab B" "Boolean Option"))
        (mult-val     (op-value "Tab B" "Multi Choice Option"))
        (string-val   (op-value "Tab B" "String Option"))
        (date-val     (gnc:date-option-absolute-time
                       (op-value "Tab B" "Just a Date Option")))
        (rel-date-val (gnc:date-option-absolute-time
                       (op-value "Tab B" "Relative Date Option")))
        (combo-date-val (gnc:date-option-absolute-time
                         (op-value "Tab B" "Combo Date Option")))
        (num-val      (op-value "Tab B" "Number Option"))
        (bg-color     (op-value "Tab B" "Background Color"))
        (accounts     (op-value "Tab A"   "An account list option"))
        (list-val     (op-value "Tab A"   "A list option"))
        (radio-val    (op-value "Tab A"   "A Radio Button option"))
        (report-title (op-value "Testing" optname-report-title))
        
        ;; document will be the HTML document that we return.
        (document (gnc:make-html-document)))

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; qof-print-date
    (let ((time-string (gnc-print-time64 (current-time) "%X"))
          (date-string (gnc-print-time64 date-val "%x"))
          (rel-date-string (gnc-print-time64 rel-date-val "%x"))
          (combo-date-string (gnc-print-time64 combo-date-val "%x")))

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
       'attribute (list "bgcolor" (format #f "#~a" bg-color)))
      
      ;; the title of the report will be rendered by the 
      ;; selected style sheet.  All we have to do is set it in the
      ;; HTML document.
      
      ;; Note we invoke the _ function upon this string.
      ;; The _ function works the same way as in C -- if a
      ;; translation of the given string is available for the
      ;; current locale, then the translation is returned,
      ;; otherwise the original string is returned.
      (gnc:html-document-set-title! document report-title)

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
         (gnc:html-markup/format (format #f
          (G_ "This is a sample GnuCash ~a report. \
See the guile (scheme) source code in the scm/report directory \
for details on writing your own reports, \
or extending existing reports.")
           gnc:version)))
        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "For help on writing reports, or to contribute your brand \
new, totally cool report, consult the mailing list ~a.")
          (gnc:html-markup-anchor 
           "mailto:gnucash-devel@gnucash.org"
           (gnc:html-markup-tt "gnucash-devel@gnucash.org")))
         (G_ " For details on subscribing to that list, see &lt;https://www.gnucash.org/&gt;.")
         (G_ " You can learn more about writing scheme at &lt;https://www.scheme.com/tspl2d/&gt;."))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The current time is ~a.") 
          (gnc:html-markup-b time-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The boolean option is ~a.")
          (gnc:html-markup-b (if bool-val (G_ "true") (G_ "false")))))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The radio button option is ~a.")
          (gnc:html-markup-b radio-val)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The multi-choice option is ~a.")
          (gnc:html-markup-b (symbol->string mult-val))))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The string option is ~a.") 
          (gnc:html-markup-b string-val)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (G_ "The date option is ~a.") 
          (gnc:html-markup-b date-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format 
          (G_ "The relative date option is ~a.")
          (gnc:html-markup-b rel-date-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format 
          (G_ "The combination date option is ~a.")
          (gnc:html-markup-b combo-date-string)))

        (gnc:html-markup-p
         (gnc:html-markup/format 
          (G_ "The number option is ~a.")
          (gnc:html-markup-b (number->string num-val))))

        ;; Here we print the value of the number option formatted as
        ;; currency. When printing currency values, you should use
        ;; the function (xaccPrintAmount), which is defined in
        ;; report-utilities. This functions will format the number
        ;; appropriately in the current locale. Don't try to format
        ;; it yourself -- it will be wrong in other locales.
        (gnc:html-markup-p 
         (gnc:html-markup/format
          (G_ "The number option formatted as currency is ~a.")
          (gnc:html-markup-b
           (xaccPrintAmount
            (inexact->exact num-val)
            (gnc-default-print-info #f)))))))

      ;; you can add as many objects as you want.  Here's another 
      ;; one.  We'll make a single-column table of the selected list 
      ;; options just for grins. 
      (gnc:html-document-add-object!
       document
       (gnc:make-html-text
        (gnc:html-markup-p (G_ "Items you selected:"))))

      (if (not (null? list-val))
          (let ((table (gnc:make-html-table)))
            (for-each
             (lambda (cell)
               (gnc:html-table-append-row! table (list (symbol->string cell))))
             list-val)
            (gnc:html-table-set-style! table "table"
             'attribute (list "style" "width:200px"))
            (gnc:html-table-set-caption! table 
                                         (G_ "List items selected"))
            (gnc:html-document-add-object! document table))
          (let ((txt (gnc:make-html-text)))
            (gnc:html-text-append!
             txt
             (gnc:html-markup-p (G_ "(You selected no list items.)")))
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
            (gnc:html-markup-p (G_ "You have selected no accounts.")))))
            
      (gnc:html-document-add-object! 
       document 
       (gnc:make-html-text 
        (gnc:html-markup-anchor (gnc-build-url URL-TYPE-HELP "gnucash-guide" "") (G_ "Display help"))))

      (gnc:html-document-add-object! 
       document 
       (gnc:make-html-text 
        (gnc:html-markup-p (G_ "Have a nice day!"))))
      
      document)))

;; ------------------------------------------------------------------
;; Here we define the actual report with gnc:define-report
;; ------------------------------------------------------------------
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Sample Report")

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "af02e925d0484745afb04f16e0524e87"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name (N_ "Sample Report")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "An options example report.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-example)

 ;; The options generator function defined above.
 'options-generator options-generator
 
 ;; The rendering function defined above.
 'renderer sample-report-renderer)
 
