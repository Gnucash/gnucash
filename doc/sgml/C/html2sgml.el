;; emacs edit functions to convert "xacc-txf-categories.html" to
;; "xacc-txf-categories.sgml"
;; "xacc-txf-categories.html" was exported from the Tax Report "TXF
;; Export Init" tab and boolean: "Print extended TXF HELP messages" v 1.4.10
;; Generates sgml for v 1.5.3 with XML file format as of 2001-Mar-08

;; to convert file:
;; Open... the file: "xacc-txf-categories.html"
;; M-x load-file "html2sgml.el"
;; Save As... "xacc-txf-categories.sgml"

(beginning-of-buffer)
(query-replace "<html>" "<ARTICLE ID=\"XACC-TXF-CATEGORIES\">\n" nil)
(beginning-of-buffer)
(query-replace "</html>" "</ARTICLE>" nil)
(beginning-of-buffer)
(query-replace "<head>" "\n<ARTHEADER>\n" nil)
(beginning-of-buffer)
(query-replace "</head>" "\n</ARTHEADER>" nil)
(beginning-of-buffer)
(query-replace-regexp "title>.*title>" "TITLE>Detailed TXF Category Descriptions</TITLE>" nil)
(beginning-of-buffer)
(query-replace-regexp "<font color=\"#......\">" "" nil)
(beginning-of-buffer)
(query-replace-regexp "</font>" "" nil)
(beginning-of-buffer)
(query-replace-regexp "<.*body.*>" "" nil)
(beginning-of-buffer)
(query-replace "b>" "EMPHASIS>" nil)
(beginning-of-buffer)
(query-replace "tr>" "ROW>" nil)
(beginning-of-buffer)
(query-replace "th>" "ENTRY>" nil)
(beginning-of-buffer)
(query-replace-regexp "<td align=left>" "<ENTRY>" nil)
(beginning-of-buffer)
(query-replace "td>" "ENTRY>" nil)
(beginning-of-buffer)
(query-replace-regexp "</center><p>.*</p>" "" nil)
(beginning-of-buffer)
(query-replace-regexp "<p>.*txf-account</p>" "" nil)
(beginning-of-buffer)
(query-replace-regexp "<table border=.*</caption>" "<PARA>\n<TABLE>\n<TITLE>Detailed TXF Category Descriptions</TITLE>\n<TGROUP COLS=\"1\">\n<THEAD>" nil)
(beginning-of-buffer)
(query-replace-regexp "<ROW><ENTRY>Tax " "<ROW>\n<ENTRY><PARA><EMPHASIS>Tax " nil)
(beginning-of-buffer)
(query-replace-regexp " Income Expense</ENTRY>\n</ROW>\n<ROW" "</ENTRY>\n</ROW>\n</THEAD>\n<TBODY>\n<ROW" nil)
(beginning-of-buffer)
(query-replace "</table>" "</TBODY>\n</TGROUP>\n</TABLE>\n</PARA>" nil)
(beginning-of-buffer)
(query-replace-regexp "\222" "'" nil)
(beginning-of-buffer)
(query-replace "<p>" "" nil)
(beginning-of-buffer)
(query-replace "</p>" "" nil)
;; EXECUTE the following macro only on the first "<br>"
(beginning-of-buffer)
(setq last-kbd-macro (read-kbd-macro
"C-s < br > <left> 2*<backspace> /PARA > < PARA C-s < / <return> PARA > < / C-a C-s \\ <return> < /EMPHASIS >"))
(call-last-kbd-macro)
(end-of-buffer)
(insert "\n<!-- Local variables: -->\n"
	"<!-- sgml-parent-document: \"gnucash.sgml\" -->\n"
	"<!-- End: -->\n")
;; EXECUTE the following macro only on all of the rest of the "<br>"s
(beginning-of-buffer)
;; NOTE: If there are less than 350 "<br>"'s and the last one is
;; replaced, this will fail, ring the bell, and terminate the execution
;; on the this file. SO, this should be the LAST insctuction in this
;; file!
;; IF IT DOES NOT RING THE BELL, SOME <br>'s WERE PROBABLY NOT REPLACED!
;; INCREASE THE "350" COUNT BELOW AND RUN AGAIN!
(execute-kbd-macro (read-kbd-macro
"C-s < br > <left> 2*<backspace> /PARA > < PARA C-a C-s R Y > < <return> PARA > < C-a C-s < / E N <return> 2*<left> PARA > < /") 350)
