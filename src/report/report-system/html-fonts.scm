;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  html-fonts.scm
;;  html stuff for fonts/css
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;  Copyright (c) Phil Longstaff <plongstaff@rogers.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash gettext))

;; Converts a font name to css style information
(define (font-name-to-style-info font-name)
    (let*
	  (
	    (font-family "Arial")
	    (font-size "20")
		(font-style #f)
		(font-style-idx 0)
		(font-weight #f)
		(font-weight-idx 0)
		(result "")
		(len (string-length font-name))
		(idx 0)
	  )
	(set! idx (string-index-right font-name #\space))
	(set! font-size (substring font-name (+ idx 1) len))
	(set! font-name (string-take font-name idx))
	(set! font-weight-idx (string-contains-ci font-name " bold"))
	(if font-weight-idx
	    (begin
		    (set! font-weight "bold")
			(set! font-name (string-append (string-take font-name font-weight-idx)
			                               (string-drop font-name (+ font-weight-idx 5))))
		))
	(set! font-style-idx (string-contains-ci font-name " italic"))
	(if font-style-idx
	    (begin
		    (set! font-style "italic")
			(set! font-name (string-append (string-take font-name font-style-idx)
			                               (string-drop font-name (+ font-style-idx 7))))
		)
		(begin
			(set! font-style-idx (string-contains-ci font-name " oblique"))
			(if font-style-idx
				(begin
					(set! font-style "oblique")
					(set! font-name (string-append (string-take font-name font-style-idx)
												   (string-drop font-name (+ font-style-idx 8))))
		))))
	(set! font-family font-name)
	(set! result (string-append
		"font-family: " font-family "; "
		"font-size: " font-size "pt; "
		(if font-style (string-append "font-style: " font-style "; ") "")
		(if font-weight (string-append "font-weight: " font-weight "; ") "")))
	result
    ))

;; Registers font options
(define (register-font-options options)
    (let*
        (
            (opt-register 
                (lambda (opt) (gnc:register-option options opt)))
            (font-family (gnc-get-default-report-font-family))
        )
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Title") "a" (N_ "Font info for the report title.")
                (string-append font-family " Bold 15")))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Account link") "b" (N_ "Font info for account name.")
                (string-append font-family " Italic 10")))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Number cell") "c" (N_ "Font info for regular number cells.")
                (string-append font-family " 10")))
        (opt-register
            (gnc:make-simple-boolean-option
                (N_ "Fonts")
                (N_ "Negative Values in Red") "d" (N_ "Display negative values in red.")
                #t))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Number header") "e" (N_ "Font info for number headers.")
                (string-append font-family " 10")))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Text cell") "f" (N_ "Font info for regular text cells.")
                (string-append font-family " 10")))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Total number cell") "g" (N_ "Font info for number cells containing a total.")
                (string-append font-family " Bold 12")))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Total label cell") "h" (N_ "Font info for cells containing total labels.")
                (string-append font-family " Bold 12")))
        (opt-register
            (gnc:make-font-option
                (N_ "Fonts")
                (N_ "Centered label cell") "i" (N_ "Font info for centered label cells.")
                (string-append font-family " Bold 12")))
    )
)

;; Adds CSS style information to an html document
(define (add-css-information-to-doc options ssdoc)
    (let*
        ((opt-val 
            (lambda (section name)
                (gnc:option-value (gnc:lookup-option options section name))))
        (negative-red? (opt-val "Fonts" "Negative Values in Red"))
        (alternate-row-color
         (gnc:color-option->html
          (gnc:lookup-option options
                     "Colors"
                     "Alternate Table Cell Color")))
        (title-font-info (font-name-to-style-info (opt-val "Fonts" "Title")))
        (account-link-font-info (font-name-to-style-info (opt-val "Fonts" "Account link")))
        (number-cell-font-info (font-name-to-style-info (opt-val "Fonts" "Number cell")))
        (number-header-font-info (font-name-to-style-info (opt-val "Fonts" "Number header")))
        (text-cell-font-info (font-name-to-style-info (opt-val "Fonts" "Text cell")))
        (total-number-cell-font-info (font-name-to-style-info (opt-val "Fonts" "Total number cell")))
        (total-label-cell-font-info (font-name-to-style-info (opt-val "Fonts" "Total label cell")))
        (centered-label-cell-font-info (font-name-to-style-info (opt-val "Fonts" "Centered label cell"))))

        (gnc:html-document-set-style-text!
            ssdoc
            (string-append
                "h3 { " title-font-info " }\n"
                "a { " account-link-font-info " }\n"
                "body, p, table, tr, td { text-align: left; " text-cell-font-info " }\n"
                "tr.alternate-row { background: " alternate-row-color " }\n"
                "th.column-heading-left { text-align: left; " number-header-font-info " }\n"
                "th.column-heading-center { text-align: center; " number-header-font-info " }\n"
                "th.column-heading-right { text-align: right; " number-header-font-info " }\n"
                "td.neg { " (if negative-red? "color: red; " "") " }\n"
                "td.number-cell, td.total-number-cell { text-align: right; white-space: nowrap; }\n"
                "td.date-cell { white-space: nowrap; }\n"
                "td.anchor-cell { white-space: nowrap; " text-cell-font-info " }\n"
                "td.number-cell { " number-cell-font-info " }\n"
                "td.number-header { text-align: right; " number-header-font-info " }\n"
                "td.text-cell { " text-cell-font-info " }\n"
                "td.total-number-cell { " total-number-cell-font-info " }\n"
                "td.total-label-cell { " total-label-cell-font-info " }\n"
                "td.centered-label-cell { text-align: center; " centered-label-cell-font-info " }\n"
            )
        )
    )
)
