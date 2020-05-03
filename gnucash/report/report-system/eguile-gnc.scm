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
(use-modules (ice-9 local-eval))  ; for the-environment
(use-modules (gnucash app-utils)) ; for _
(use-modules (gnucash utilities)) ; for gnc:html-string-sanitize

(define-public (string-substitute-alist str sub-alist)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         (display
          (or (assv-ref sub-alist c)
              c)))
       str))))

;; regexps used to find start and end of code segments
(define startre (and (defined? 'make-regexp) (make-regexp "<\\?scm(:d)?[[:space:]]")))
(define endre   (and (defined? 'make-regexp) (make-regexp "(^|[[:space:]])\\?>")))

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
  ;; enclosing quotes that (format) insists on adding.
  ;; (can't use (write) either because that wraps each line of output
  ;; in double quotes)
  (define (display-text t)
    (let ((esct (format #f "~s" t)))
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
    (cond
     (code? (display textstart)
            (set! stop textstop))
     (dmodifier? (display dcodestart)
                 (set! stop dcodestop))
     (else (display codestart)
           (set! stop codestop))))

  ;; recursively process input stream
  (define (loop inp needle other code? line)
    (when (string-null? line)
      (set! line (read-line inp 'concat)))
    (unless (eof-object? line)
      (cond
       ((regexp-exec needle line)
        => (lambda (rmatch)
             (let ((dmodifier? #f))
               (display-it (match:prefix rmatch) code?)
               (unless code?
                 ;; switching from text to code -- check for modifier
                 (set! dmodifier? (match:substring rmatch 1)))
               (switch-mode code? dmodifier?)
               (loop inp other needle (not code?) (match:suffix rmatch)))))
       (else    ; no match - output whole line and continue
        (display-it line code?)
        (loop inp needle other code? "")))))

  (display textstart)
  (if (defined? 'make-regexp)
      (loop (current-input-port) startre endre #f "")
      (display "eguile requires guile with regex."))
  (display stop))

;end of (template->script)

;; Evaluate input containing Scheme code, trapping errors
;; e.g. (display "Text ")(display (+ x 2))(display ".") -> Text 42.
;; Parameters:
;;   env  - environment in which to do the evaluation;
;;          if #f, (the-environment) will be used
(define (script->output env)

  ;; Placeholder for the error stack in case of an error
  (define error-stack #f)
  (define local-env (or env (the-environment)))

  ;; Actual evaluation function. This is where the work happens.
  (define (eval-input)
    (let lp ((next (read)))
      (cond
       ((eof-object? next) #f)
       (else
        (local-eval next local-env)
        (lp (read))))))

  ;; Error handler to display any errors while evaluating the template
  (define (error-handler key subr message args . rest)
    (display "<p>")
    (display (_ "An error occurred when processing the template:"))
    (display "<br/><pre>")
    (display
     (gnc:html-string-sanitize
      (with-output-to-string
        (lambda ()
          (display-backtrace error-stack (current-output-port))
          (newline)
          (display-error #f (current-output-port) subr message args rest)))))
    (display "</pre><br/>"))

  (define (pre-unwind-handler key . rest)
    ;; Capture the stack here, cut the last 3 frames which are
    ;; make-stack, this one, and the throw handler.
    (set! error-stack (make-stack #t 3)))

  (catch #t eval-input error-handler pre-unwind-handler))

; end of (script->output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Process a template file and return the result as a string
(define (eguile-file-to-string infile environment)
  (cond
   ((not (access? infile R_OK))
    (format #f (_ "Template file \"~a\" can not be read") infile))
   (else
    (let ((script (with-input-from-file infile
                    (lambda ()
                      (with-output-to-string template->script)))))
      (with-output-to-string
        (lambda ()
          (with-input-from-string script
            (lambda ()
              (script->output environment)))))))))

(export eguile-file-to-string)

