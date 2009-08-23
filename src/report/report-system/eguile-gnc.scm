;;
;; eguile-gnc.scm -- embedded guile preprocessor for GnuCash
;; Copyright (c) 2009 Chris Dennis <chris@starsoftanalysis.co.uk>
;; Based on eguile.scm by Neale Pickett <neale@woozle.org>
;; (see http://woozle.org/~neale/src/eguile/)
;;
;; $Author: chris $ $Date: 2009/06/19 22:44:43 $ $Revision: 1.7 $
;;
;; Why all the changes from the original eguile?
;;   - need to escape " etc in text
;;   - single pass template parsing - allow use as filter (still need
;;       another pass for evaluation)
;;   - regexps to allow any whitespace, not just 'space'
;;   - catch exceptions
;;   - make it a module as part of the GnuCash directory structure
;;    
;;
;; Documentation
;; -------------
;;
;; eguile-gnc will process a file containing text and embedded Guile code.
;;
;; The text may well be HTML, but could be anything.  I'll use HTML for
;; the examples here.
;;
;; Guile/Scheme code is wrapped in  '<?scm ... ?>'
;; (whitespace is required after '<?scm' and before '?>')
;;
;; '<?scm's can NOT be nested
;;
;; The optional :d modifier (i.e. '<?scm:d' ) is just a shortcut for '(display ... )'
;; so '<?scm:d x ?>' is the same as '<?scm (display x) ?>'
;;
;; Note that s-expressions can be spread across more than one '<?scm ... ?>', 
;; for example:
;;    <?scm (if (> x 3) (begin ?>Bigger<?scm ) (begin ?>Smaller<?scm )) ?>
;;
;; Each chunk of text outside a '<?scm ... ?>' ends up wrapped
;; in a (display ... ), after having had double quotes etc. escaped.
;;
;; The processing happens in two passes.  Initially the input file is converted
;; to a Guile script, and then that script is evaluated to produce the final
;; result.
;;
;; For example, if the input file contained these lines:
;;   
;;   <h1 align="center">Invoice <?scm:d invoiceid ?></h1>
;;   <?scm (for-each (lambda (entry) ?>
;;     <p>Date: <?scm:d (entry date) ?>, description: <?scm:d (entry desc) ?>
;;   <?scm ) entries) ?>
;;
;; the resulting script would look like:
;;
;;   (display "<h1 align=\"center\">Invoice ")(display invoiceid)(display "</h1>")
;;   (for-each (lambda (entry)
;;     (display "<p>Date: ")(display (entry date))
;;     (display ", description: ")(display (entry desc))
;;   ) entries)
;;   
;; and the final result might be this string:
;;
;;   "<h1 align=\"center\">Invoice 002345</h1>
;;    <p>Date: 04/03/2009, description: Widgets
;;    <p>Date: 05/03/2009, description: Modified widgets"
;;
;; 

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307 USA

(define-module (gnucash report eguile-gnc))

(use-modules (ice-9 regex))       ; for regular expressions
(use-modules (ice-9 rdelim))      ; for read-line
(use-modules (gnucash app-utils)) ; for _

;; This is needed for displaying error messages -- note that it assumes that
;; the output is HTML, which is a pity, because otherwise this module is
;; non-specific -- it is designed to output a mixture of Guile and any other
;; sort of text.  Oh well.
(define (escape-html s1) 
  ;; convert string s1 to escape HTML special characters < > and &
  ;; i.e. convert them to &lt; &gt; and &amp; respectively.
  ;; Maybe there's a way to do this in one go... (but order is important)
  (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
  (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
  (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))

;; regexps used to find start and end of code segments
(define startre (make-regexp "<\\?scm(:d)?[[:space:]]"))
(define endre   (make-regexp "(^|[[:space:]])\\?>"))

;; Guile code to mark starting and stopping text or code modes
(define textstart  "(display \"")
(define textstop   "\")")
(define codestart  "")
(define codestop   "")
(define dcodestart "(display ")
(define dcodestop  ")")

;; Parse a template, and return a sequence of s-expressions
;; e.g. "Text <?scm:d (+ x 2) ?>." -> (display "Text ")(display (+ x 2))(display ".")
(define (template->script)

  ;; output text with double quotes escaped, but without the outer
  ;; enclosing quotes that (simple-format) insists on adding.
  ;; (can't use (write) either because that wraps each line of output
  ;; in double quotes)
  (define (display-text t)
    (let ((esct (simple-format #f "~s" t)))
      (display (substring esct 1 (- (string-length esct) 1)))))

  ;; display either code or text
  (define (display-it t code?)
    (if code?
      (display t)
      (display-text t)))  

  (define stop textstop)    ; text to output at end of current section

  ;; switch between code and text modes
  (define (switch-mode code? dmodifier?)
    (display stop)
    (if code?
      (begin ; code mode to text mode
        (display textstart)
        (set! stop textstop))
      (begin ; text mode to code mode
        (if dmodifier?
          (begin
            (display dcodestart)
            (set! stop dcodestop))
          (begin
            (display codestart)
            (set! stop codestop))))))

  ;; recursively process input stream
  (define (loop inp needle other code? line)
    (if (eq? line "")
      (set! line (read-line inp 'concat)))
    (if (not (eof-object? line)) 
      (let ((match (regexp-exec needle line)))
        (if match
          (let ((dmodifier? #f))
            (display-it (match:prefix match) code?)
            (if (not code?)
              ; switching from text to code -- check for modifier
              (set! dmodifier? (match:substring match 1)))
            (switch-mode code? dmodifier?)
            (loop inp other needle (not code?) (match:suffix match)))
          (begin    ; no match - output whole line and continue
            (display-it line code?)
            (loop inp needle other code? ""))))))

  (display textstart)
  (loop (current-input-port) startre endre #f "")
  (display stop))

;end of (template->script)

;; Evaluate input containing Scheme code, trapping errors
;; e.g. (display "Text ")(display (+ x 2))(display ".") -> Text 42.
;; Parameters:
;;   env  - environment in which to do the evaluation; 
;;          if #f, (the-environment) will be used
(define (script->output env)
  (define (eval-input)
    (let ((s-expression (read)))
      (while (not (eof-object? s-expression))
             (local-eval s-expression (or env (the-environment))) 
             (set! s-expression (read)))))

  (define (error-handler key subr message args . rest)
    (display "<p>")
    (display (_ "An error occurred when processing the template:"))
    (display "<br>")
    (display
      (escape-html
        (with-output-to-string
          (lambda ()
            (display-error #f (current-output-port) subr message args rest)))))
    (display "<br>"))

  (catch #t eval-input error-handler))
; end of (script->output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Process a template file and return the result as a string
(define (eguile-file-to-string infile environment)
  (if (not (access? infile R_OK))  
    (sprintf #f (_ "Template file \"%s\" can not be read") infile)
    (let ((script (with-input-from-file
                    infile
                    (lambda () (with-output-to-string template->script)))))
      (with-output-to-string
        (lambda () (with-input-from-string 
                     script
                     (lambda () (script->output environment))))))))

(export eguile-file-to-string)

