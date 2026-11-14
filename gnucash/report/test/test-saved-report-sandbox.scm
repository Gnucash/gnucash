;; Tests for the sandbox used to load saved reports. This only needs the
;; sandbox module, not a running GnuCash, so the "GnuCash" procedures that
;; saved reports call are replaced here by recording stubs.

(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash report saved-report-sandbox))

(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "saved-report-sandbox")
  (test-serialized-report-loads)
  (test-embedded-report-loads)
  (test-hostile-forms-are-refused)
  (test-set!-is-local-only)
  (test-load-file)
  (test-end "saved-report-sandbox"))

;; What the stubs were called with, most recent first.
(define calls '())
(define (record! . args) (set! calls (cons args calls)))

(define (make-stubs)
  (set! calls '())
  (list
   (cons 'gnc:menuname-custom "_Custom")
   (cons 'gnc:report-template-new-options/report-guid
         (lambda (id name) (record! 'new-options id name) (list 'options id)))
   (cons 'gnc:report-template-renderer/report-guid
         (lambda (id name) (lambda (report) 'rendered)))
   (cons 'gnc:lookup-option
         (lambda (options section name) (list 'option section name)))
   (cons 'gnc:option-set-value
         (lambda (option value) (record! 'set-value option value)))
   (cons 'gnc:option-setter
         (lambda (option) (lambda (value) (record! 'setter option value))))
   (cons 'gnc:restore-report-by-guid-with-custom-template
         (lambda (id type name custom options)
           (record! 'restore type name)
           (length calls)))
   (cons 'gnc:optiondb (lambda (o) o))
   (cons 'gnc-lookup-option (lambda (db section name) (list 'option section name)))
   ;; the saved report-list: one (id . options) entry per embedded report
   (cons 'GncOption-get-value (lambda (o) (list (cons 0 '("sub" "Sub")))))
   (cons 'gnc:define-report
         (lambda args
           (record! 'define-report args)
           'the-record))))

(define (make-env) (make-saved-report-sandbox (make-stubs)))

;; Shaped exactly like gnc:report-template-serialize-internal output.
(define serialized-report
  ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options for saved report \"My \\\"quoted\\\" report\", based on template \"abcd\"
(let ()
  (define (options-gen)
    (let
         (
           (options (gnc:report-template-new-options/report-guid \"abcd\" \"Balance Sheet\"))
           (new-embedded-report-ids '()) ;; only used with Multicolumn View Reports
         )
; Section: General

(let ((option (gnc:lookup-option options
                                 \"General\"
                                 \"Report name\")))
  ((lambda (o) (if o (gnc:option-set-value o \"My \\\"quoted\\\" report\"))) option))

(let ((option (gnc:lookup-option options
                                 \"Accounts\"
                                 \"Accounts\")))
  ((lambda (o) (if o (gnc:option-set-value o '(\"guid1\" \"guid2\")))) option))

(let ((option (gnc:lookup-option options
                                 \"Display\"
                                 \"Show zero\")))
  ((lambda (o) (if o (gnc:option-set-value o #t))) option))

      options
    )
  )
  (gnc:define-report
    'version 1
    'name \"My \\\"quoted\\\" report\"
    'report-guid \"0123456789abcdef0123456789abcdef\"
    'parent-type \"abcd\"
    'options-generator options-gen
    'menu-path (list gnc:menuname-custom)
    'renderer (gnc:report-template-renderer/report-guid \"abcd\" \"Balance Sheet\")
  )
)

")

;; The embedded-report part uses set! on a local variable.
(define embedded-fragment
  "(let ()
  (define (options-gen)
    (let ((options (gnc:report-template-new-options/report-guid \"mc\" \"Multicolumn\"))
          (new-embedded-report-ids '()))
      (let ((options (gnc:report-template-new-options/report-guid \"sub\" \"Sub\")))
        (let ((option (gnc:lookup-option options \"General\" \"Report name\")))
          ((lambda (o) (if o (gnc:option-set-value o \"Sub\"))) option))
        (set! new-embedded-report-ids
          (append new-embedded-report-ids
                  (list (gnc:restore-report-by-guid-with-custom-template #f \"sub\" \"Sub\" \"cust\" options)))))
      (let*
        ((option (gnc-lookup-option (gnc:optiondb options) \"__general\" \"report-list\"))
         (saved-report-list (GncOption-get-value option)))
        ((lambda (option)
           (if option ((gnc:option-setter option)
                       (map (lambda (x y) (cons x (cdr y))) new-embedded-report-ids saved-report-list))))
         option))
      options))
  (gnc:define-report 'version 1 'name \"MC\" 'report-guid \"ffff\"
                     'parent-type \"mc\" 'options-generator options-gen))")

(define (test-serialized-report-loads)
  (test-begin "serialized-report")
  (let ((result (sandbox-eval-string serialized-report (make-env))))
    (test-equal "value of the last form is returned" 'the-record result)
    (let ((def (cadr (assq 'define-report calls))))
      (test-equal "report name survives quoting"
        "My \"quoted\" report" (cadr (memq 'name def)))
      (test-equal "report guid"
        "0123456789abcdef0123456789abcdef" (cadr (memq 'report-guid def)))
      (test-equal "menu path" '("_Custom") (cadr (memq 'menu-path def)))
      ;; the options generator is lazily run later, still inside the sandbox
      (let ((gen (cadr (memq 'options-generator def))))
        (test-equal "options generator runs and returns options"
          '(options "abcd") (gen))
        (test-equal "three options were set"
          3 (length (filter (lambda (c) (eq? (car c) 'set-value)) calls))))))
  (test-end "serialized-report"))

(define (test-embedded-report-loads)
  (test-begin "embedded-report")
  (sandbox-eval-string embedded-fragment (make-env))
  (let* ((def (cadr (assq 'define-report calls)))
         (gen (cadr (memq 'options-generator def))))
    (test-equal "generator returns options" '(options "mc") (gen))
    (test-assert "embedded report was restored (set! on a local works)"
      (assq 'restore calls))
    (test-assert "report-list option was set from the new ids"
      (assq 'setter calls)))
  (test-end "embedded-report"))

;; A file name that does not exist (nor is created by this test unless
;; something escapes the sandbox).
(define (temp-name)
  (let* ((port (mkstemp! (string-append (or (getenv "TMPDIR") "/tmp")
                                        "/gnc-sandbox-XXXXXX")))
         (name (port-filename port)))
    (close-port port)
    (delete-file name)
    name))

(define sentinel (temp-name))

(define hostile-forms
  `("(system \"echo x\")"
    ,(string-append "(system (string-append \"touch \" \"" sentinel "\"))")
    ,(string-append "((@@ (guile) system) \"touch " sentinel "\")")
    ,(string-append "((@ (ice-9 popen) open-pipe) \"touch " sentinel "\" OPEN_READ)")
    ,(string-append "(call-with-output-file \"" sentinel "\" (lambda (p) #t))")
    "(open-file \"/etc/passwd\" \"r\")"
    "(primitive-load \"/etc/passwd\")"
    "(load \"/etc/passwd\")"
    "(eval '(+ 1 2) (current-module))"
    "(eval-string \"(+ 1 2)\")"
    "(use-modules (ice-9 popen))"
    "(resolve-module '(guile))"
    "(module-ref (current-module) 'system)"
    "(getenv \"HOME\")"
    "(exit 0)"
    "(quit)"
    "(define-syntax foo (lambda (x) (system \"echo x\"))) (foo)"
    "(eval-when (expand eval compile) (system \"echo x\"))"
    ;; defining a closure is allowed, but calling it must fail (see below)
    "(gnc:define-report 'renderer (lambda (r) (system \"echo x\")))"
    "(let loop () (loop))"))

(define (test-hostile-forms-are-refused)
  (test-begin "hostile-forms")
  (for-each
   (lambda (form)
     ;; a render-time call is as dangerous as a load-time one, so for the
     ;; one form that merely *defines* a closure, call it
     (let ((outcome
            (catch #t
              (lambda ()
                (let ((r (sandbox-eval-string form (make-env))))
                  (when (and (pair? calls) (eq? (car (car calls)) 'define-report))
                    ((cadr (memq 'renderer (cadr (car calls)))) #f))
                  'ran))
              (lambda args 'refused))))
       (test-equal (string-append "refused: " form) 'refused outcome)))
   hostile-forms)
  (test-assert "nothing was written or executed on the host"
    (not (file-exists? sentinel)))
  (test-assert "reader refuses #. evaluation"
    (catch #t
      (lambda () (sandbox-eval-string "#.(system \"echo x\")" (make-env)) #f)
      (lambda args #t)))
  (test-equal "host bindings untouched" 1 (car '(1)))
  (test-end "hostile-forms"))

(define (test-set!-is-local-only)
  (test-begin "set!")
  (test-equal "set! on a let variable" '(1)
    (sandbox-eval-string "(let ((n '())) (set! n (cons 1 n)) n)" (make-env)))
  (test-equal "set! on an internal define" 2
    (sandbox-eval-string "(let () (define n 1) (set! n 2) n)" (make-env)))
  (for-each
   (lambda (form)
     (test-assert (string-append "refused: " form)
       (catch #t
         (lambda () (sandbox-eval-string form (make-env)) #f)
         (lambda args #t))))
   '("(set! car cdr)"
     "(set! gnc:define-report 1)"
     "(define zz 1) (set! zz 2)"))
  (test-equal "host car still works" 1 (car '(1)))
  (test-end "set!"))

(define (write-file name text)
  (call-with-output-file name (lambda (p) (display text p))))

(define (test-load-file)
  (test-begin "load-file")
  (let* ((file (temp-name))
         (warnings '())
         (warn (lambda (m) (set! warnings (cons m warnings)))))
    ;; good, bad, good: the bad one is skipped, the rest are loaded
    (write-file file
                (string-append serialized-report
                               "(system \"echo x\")\n"
                               embedded-fragment "\n"))
    (test-assert "file loads" (sandbox-load-file file (make-env) warn))
    (test-equal "two reports defined" 2
      (length (filter (lambda (c) (eq? (car c) 'define-report)) calls)))
    (test-equal "one warning, for the refused form" 1 (length warnings))
    (test-assert "warning names the file and the problem"
      (and (string-contains (car warnings) file)
           (string-contains (car warnings) "system")))

    ;; unbalanced input: reports before the damage still load
    (set! warnings '())
    (write-file file (string-append serialized-report "(let ((x 1)\n"))
    (test-assert "truncated file still loads" (sandbox-load-file file (make-env) warn))
    (test-equal "first report defined" 1
      (length (filter (lambda (c) (eq? (car c) 'define-report)) calls)))
    (test-equal "one warning for the unreadable tail" 1 (length warnings))

    (delete-file file)
    (set! warnings '())
    (test-equal "missing file returns #f" #f
      (sandbox-load-file file (make-env) warn))
    (test-equal "missing file is reported" 1 (length warnings)))
  (test-end "load-file"))
