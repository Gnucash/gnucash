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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (gnucash main))

(use-modules (ice-9 slib))
(use-modules (g-wrapped gw-gnc))
(use-modules (g-wrapped gw-runtime))

;; Load the srfis (eventually, we should see where these are needed
;; and only have the use-modules statements in those files).
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))

(use-modules (gnucash gnc-module))
(use-modules (ice-9 slib))
(require 'printf)

;; from bootstrap.scm
(export gnc:version)
(export gnc:debugging?)
(export gnc:warn)
(export gnc:error)
(export gnc:msg)
(export gnc:debug)
(export build-path)
(export gnc:use-module-here!)
(export hash-fold)
(export item-list->hash!)
(export string-split)
(export string-join)
(export gnc:backtrace-if-exception)

;; from main.scm
(export gnc:main)

;; from path.scm
(export gnc:make-home-dir)
(export gnc:current-config-auto)

;; from command-line.scm
(export gnc:*command-line-remaining*)
(export gnc:*config-dir*)
(export gnc:*share-dir*)

;; from doc.scm
(export gnc:find-doc-file)
(export gnc:load-help-topics)

;; from main-window.scm
(export gnc:find-acct-tree-window-options)
(export gnc:make-new-acct-tree-window)  
(export gnc:free-acct-tree-window)
(export gnc:main-window-save-state)

;; from printing/print-check.scm
(export make-print-check-format)
(export gnc:print-check)

;; from tip-of-the-day.scm
(export gnc:get-current-tip)
(export gnc:increment-tip-number)
(export gnc:decrement-tip-number)

;; Get the Makefile.am/configure.in generated variables.
(load-from-path "build-config.scm")

;; Do this stuff very early -- but other than that, don't add any
;; executable code until the end of the file if you can help it.
;; These are needed for a guile 1.3.4 bug
(debug-enable 'debug)
(read-enable 'positions)

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

;;(use-modules (ice-9 statprof))

;; A list of things to do when in batch mode after the initial
;; startup.  List items may be strings, in which case they're read and
;; evaluated or procedures, in which case they're just executed.
;; The items will be done in reverse order.

(define gnc:*batch-mode-things-to-do* '())

;; These will be converted to config vars later (see command-line.scm)
(define gnc:*debugging?* (if (getenv "GNC_DEBUG") #t #f))
(define gnc:*develmode* (if (getenv "GNC_DEVEL_MODE") #t #f))

;; Function to get debugging
(define (gnc:debugging?)
  (if (boolean? gnc:*debugging?*)
      gnc:*debugging?*
      (gnc:config-var-value-get gnc:*debugging?*)))

(define (gnc:setup-debugging)
  (if (gnc:debugging?)
      (debug-enable 'backtrace)))

;; various utilities

;; Test for simple-format
(if (not (defined? 'simple-format))
    (begin
      (require 'format)
      (export simple-format)
      (define simple-format format)))

(define gnc:use-guile-module-here!
  ;; FIXME: this should be a temporary fix.  We need to check to see
  ;; if there's a more approved way to do this.  As I recall, there's
  ;; not, but I belive a better way will be added to Guile soon.

  ;; module arg must be something like '(ice-9 slib)
  (cond
   ((or (string=? "1.3" (version))
        (string=? "1.3.4" (version))
        (string=? "1.4" (version)))
    (lambda (module)
      (process-use-modules (list module))))
   (else
    (lambda (module)
      (process-use-modules (list (list module)))))))

(if (not (defined? 'hash-fold))
    (define (hash-fold proc init table)
      (for-each 
       (lambda (bin)
         (for-each 
          (lambda (elt)
            (set! init (proc (car elt) (cdr elt) init)))
          bin))
       (vector->list table))))

(define (item-list->hash! lst hash
			  getkey getval
			  hashref hashset 
			  list-duplicates?)
  ;; Takes a list of the form (item item item item) and returns a hash
  ;; formed by traversing the list, and getting the key and val from
  ;; each item using the supplied get-key and get-val functions, and
  ;; building a hash table from the result using the given hashref and
  ;; hashset functions.  list-duplicates? determines whether or not in
  ;; the resulting hash, the value for a given key is a list of all
  ;; the values associated with that key in the input or just the
  ;; first one encountered.

  (define (handle-item item)
    (let* ((key (getkey item))
	   (val (getval item))
	   (existing-val (hashref hash key)))

      (if (not list-duplicates?)
	  ;; ignore if not first value.
	  (if (not existing-val) (hashset hash key val))
	  ;; else result is list.
	  (if existing-val
	      (hashset hash key (cons val existing-val))
	      (hashset hash key (list val))))))
	      
  (for-each handle-item lst)
  hash)

(define (string-join lst joinstr)
  ;; This should avoid a bunch of unnecessary intermediate string-appends.
  ;; I'm presuming those are more expensive than cons...
  (if (or (not (list? lst)) (null? lst))
      ""
      (apply string-append
             (car lst)
             (let loop ((remaining-elements (cdr lst)))
               (if (null? remaining-elements)
                   '()
                   (cons joinstr (cons (car remaining-elements)
                                       (loop (cdr remaining-elements)))))))))

(define (string-split str char)
  (let ((parts '())
        (first-char #f))
    (let loop ((last-char (string-length str)))
      (set! first-char (string-rindex str char 0 last-char))
      (if first-char 
          (begin 
            (set! parts (cons (substring str (+ 1 first-char) last-char) 
                              parts))
            (loop first-char))
          (set! parts (cons (substring str 0 last-char) parts))))    
    parts))

(define (gnc:backtrace-if-exception proc . args)
  (define (dumper key . args)
    (let ((stack (make-stack #t dumper)))
      (display-backtrace stack (current-error-port))
      (apply display-error stack (current-error-port) args)
      (throw 'ignore)))
  
  (catch 
   'ignore
   (lambda () 
     (lazy-catch #t 
                 (lambda () (apply proc args))
                 dumper))
   (lambda (key . args)
     #f)))

;;;; Status output functions.

(define (gnc:warn . items)
  (display "gnucash: [W] ")
  (for-each (lambda (i) (write i)) items)
  (newline))

(define (gnc:error . items)
  (display "gnucash: [E] ")
  (for-each (lambda (i) (write i)) items)
  (newline))

(define (gnc:msg . items)
  (display "gnucash: [M] ")
  (for-each (lambda (i) (write i)) items)
  (newline))

(define (gnc:debug . items)
  (if (gnc:debugging?)
      (begin
        (display "gnucash: [D] ")
        (for-each (lambda (i) (write i)) items)
        (newline))))


;; Set up timing functions

(define gnc:*last-time* (gettimeofday))

(define (gnc:timestamp . stuff)
  (let* ((now (gettimeofday))
         (delta (+ (- (car now) (car gnc:*last-time*))
                   (/ (- (cdr now) (cdr gnc:*last-time*)) 1000000))))
    (gnc:msg stuff "-- Elapsed time: " delta "seconds.")
    (set! gnc:*last-time* now)))


(define (build-path firstelement . restofpath)
  (define separator "/")
  (define (bp first rest)
    (if (null? rest)
	first
	(bp 	
	 (string-append first separator (car rest))
	 (cdr rest))))
  (if (null? restofpath)
      firstelement
      (bp 
       (string-append firstelement separator
		      (car restofpath))
       (cdr restofpath))))

(define (gnc:find-in-directories file directories)
  "Find file named 'file' anywhere in 'directories'.  'file' must be a
string and 'directories' must be a list of strings."

  (gnc:debug "gnc:find-in-directories looking for " file " in " directories)

  (do ((rest directories (cdr rest))
       (finished? #f)
       (result #f))
      ((or (null? rest) finished?) result)

    (let ((file-name (build-path (car rest) file)))
      (gnc:debug "  checking for " file-name)
      (if (access? file-name F_OK)
          (begin
            (gnc:debug "found file " file-name)
            (set! finished? #t)
            (set! result file-name))))))

(define (gnc:print-unstable-message)
  (newline)
  (newline)
  (display (_ "This is a development version. It may or may not work."))
  (newline)
  (display (_ "Report bugs and other problems to gnucash-devel@gnucash.org."))
  (newline)
  (display (sprintf #f (_ "The last stable version was %s.") "GnuCash 1.6.4"))
  (newline)
  (display (sprintf #f (_ "The next stable version will be %s.")
                    "GnuCash 1.8.0"))
  (newline)
  (newline))

(define (gnc:report-menu-setup)
  ;; since this menu gets added to every child window, we say it 
  ;; comes after the "_File" menu. 
  (define menu (gnc:make-menu gnc:menuname-reports (list "_File")))
  (define menu-namer (gnc:new-menu-namer))
  (define tax-menu (gnc:make-menu gnc:menuname-taxes
                                  (list gnc:menuname-reports "")))
  (define income-expense-menu
    (gnc:make-menu gnc:menuname-income-expense
                   (list gnc:menuname-reports "")))
  (define asset-liability-menu
    (gnc:make-menu gnc:menuname-asset-liability
                   (list gnc:menuname-reports "")))
  (define utility-menu
    (gnc:make-menu gnc:menuname-utility
                   (list gnc:menuname-reports "")))
  (define menu-hash (make-hash-table 23))

  (define (add-template-menu-item name template)
    (if (gnc:report-template-in-menu? template)
        (let ((title (string-append (_ "Report") ": " (_ name)))
              (menu-path (gnc:report-template-menu-path template))
              (menu-name (gnc:report-template-menu-name template))
              (menu-tip (gnc:report-template-menu-tip template))
              (item #f))

          (if (not menu-path)
              (set! menu-path '(""))
              (set! menu-path
                    (append menu-path '(""))))

          (set! menu-path (append (list gnc:menuname-reports) menu-path))

          (if menu-name (set! name menu-name))

          (if (not menu-tip)
              (set! menu-tip
                    (sprintf #f (_ "Display the %s report") (_ name))))

          (set! item
                (gnc:make-menu-item
                 ((menu-namer 'add-name) name)
                 menu-tip
                 menu-path
                 (lambda ()
                   (let ((report (gnc:make-report
                                  (gnc:report-template-name template))))
                     (gnc:main-window-open-report report #f)))))
          (gnc:add-extension item))))

  (gnc:add-extension menu)

  ;; add the menu option to edit style sheets 
  (gnc:add-extension
   (gnc:make-menu-item 
    ((menu-namer 'add-name) (_ "Style Sheets..."))
    (_ "Edit report style sheets.")
    (list "_Settings" "")
    (lambda ()
      (gnc:style-sheet-dialog-open))))

;  (gnc:add-extension tax-menu)
  (gnc:add-extension income-expense-menu)
  (gnc:add-extension asset-liability-menu)
  (gnc:add-extension utility-menu)

  ;; push reports (new items added on top of menu)
  (gnc:report-templates-for-each add-template-menu-item)

  ;; the Welcome to Gnucash-1.6 extravaganza 
  (gnc:add-extension 
   (gnc:make-menu-item 
    ((menu-namer 'add-name) (_ "Welcome Extravaganza")) 
    (_ "Welcome-to-GnuCash screen")
    (list gnc:menuname-reports gnc:menuname-utility "")
    (lambda ()
      (gnc:make-welcome-report)))))

(define (gnc:startup)
  (gnc:debug "starting up.")
  (gnc:setup-debugging)

  (let ((envdir (getenv "GNC_CONFIG_DIR")))
    (if envdir
        (set! gnc:_install-config-dir_ envdir)))
  (let ((envdir (getenv "GNC_SHARE_DIR")))
    (if envdir
        (set! gnc:_install-share-dir_ envdir)))
  (let ((envdir (getenv "GNC_HELP_DIR")))
    (if envdir
        (set! gnc:_install-help-dir_ envdir)))

  ;; initialize the gnucash module system 
  (gnc:module-system-init)
  
  ;; SUPER UGLY HACK -- this should go away when I come back for the
  ;; second cleanup pass...
  (let ((original-module (current-module))
        (bootstrap (resolve-module '(gnucash main))))
    
    (set-current-module bootstrap)
    
    ;; right now we have to statically load all these at startup time.
    ;; Hopefully we can gradually make them autoloading.
    (gnc:module-load "gnucash/engine" 0)

    (gnc:module-load "gnucash/app-utils" 0)
    (gnc:setup-gettext)
    (setlocale LC_ALL "")

    (gnc:module-load "gnucash/app-file" 0)
    (gnc:module-load "gnucash/register/ledger-core" 0)
    (gnc:module-load "gnucash/register/register-core" 0)
    (gnc:module-load "gnucash/register/register-gnome" 0)
    (gnc:module-load "gnucash/import-export/binary-import" 0)
    (gnc:module-load "gnucash/import-export/qif-import" 0)
    (gnc:module-load "gnucash/report/report-system" 0)
    (gnc:module-load "gnucash/report/stylesheets" 0)
    (gnc:module-load "gnucash/report/standard-reports" 0)
    (gnc:module-load "gnucash/report/utility-reports" 0)
    (gnc:module-load "gnucash/report/locale-specific/us" 0)
    (gnc:module-load "gnucash/business-gnome" 0)

    ;; Now we can load a bunch of files.
    (load-from-path "path.scm")
    (load-from-path "command-line.scm")
    (load-from-path "doc.scm")
    (load-from-path "main-window.scm")
    (load-from-path "tip-of-the-day.scm")
    (load-from-path "printing/print-check.scm")

    (gnc:use-guile-module-here! '(gnucash price-quotes))
    (set-current-module original-module))

  (gnc:hook-add-dangler gnc:*book-opened-hook*
                        (lambda (file)
                          (if ((gnc:option-getter
                                (gnc:lookup-global-option
                                 "Scheduled Transactions"
                                 "Run on GnuCash start" )))
                              (gnc:sx-since-last-run-wrapper file))))

  (gnc:hook-add-dangler gnc:*new-book-hook*
                        (lambda ()
                          (let ((option (gnc:lookup-global-option
                                         "General"
                                         "No account list setup on new file")))
                            (if (and option (not (gnc:option-value option)))
                                (gnc:ui-hierarchy-druid)))))

  (if (not (gnc:handle-command-line-args))
      (gnc:shutdown 1))

  ;; Load the system configs
  (if (not (gnc:load-system-config-if-needed))
      (gnc:shutdown 1))

  ;; Load the user configs
  (gnc:load-user-config-if-needed)

  ;; Clear the change flags caused by loading the configs
  (gnc:global-options-clear-changes)

  (gnc:report-menu-setup)

  (gnc:hook-run-danglers gnc:*startup-hook*)

  (if (gnc:config-var-value-get gnc:*arg-show-version*)
      (begin
        (gnc:prefs-show-version)
        (gnc:shutdown 0)))

  (if (or (gnc:config-var-value-get gnc:*arg-show-usage*)
          (gnc:config-var-value-get gnc:*arg-show-help*))
      (begin
        (gnc:prefs-show-usage)
        (gnc:shutdown 0)))

  (if (gnc:config-var-value-get gnc:*loglevel*)
      (gnc:set-log-level-global (gnc:config-var-value-get gnc:*loglevel*))))


(define (gnc:shutdown exit-status)
  (gnc:debug "Shutdown -- exit-status: " exit-status)

  (cond ((gnc:ui-is-running?)
	 (if (not (gnc:ui-is-terminating?))
             (if (gnc:file-query-save)
                 (begin
                   (gnc:hook-run-danglers gnc:*ui-shutdown-hook*)
                   (gnc:ui-shutdown)))))
        (else
	 (gnc:ui-destroy)
	 (gnc:hook-run-danglers gnc:*shutdown-hook*)
         (gnc:engine-shutdown)
	 (exit exit-status))))

(define (gnc:ui-finish)
  (gnc:debug "UI Shutdown hook.")
  (gnc:file-quit))

(define (gnc:account-file-to-load)
  (let ((ok (not (gnc:config-var-value-get gnc:*arg-no-file*)))
        (file (if (pair? gnc:*command-line-remaining*)
                  (car gnc:*command-line-remaining*)
                  (gnc:history-get-last))))
    (and ok (string? file) file)))

(define (gnc:load-account-file)
  (let ((file (gnc:account-file-to-load)))
    (if file 
        (and (not (gnc:file-open-file file))
             (gnc:hook-run-danglers gnc:*book-opened-hook* #f))
        (gnc:hook-run-danglers gnc:*book-opened-hook* #f))))

(define (gnc:main)

  (define (handle-batch-mode-item item)
    (cond
     ((procedure? item) (item))
     ((string? item)
      (call-with-input-string
       item
       (lambda (port)
         (let loop ((next-form (read port)))
           (if (not (eof-object? next-form))
               (begin
                 ;; FIXME: is this where we want to eval these?
                 ;; should we perhaps have a (gnucash user)?
                 (eval next-form (resolve-module '(gnucash main)))
                 (loop (read port))))))))
     (else
      (display "gnucash: unknown batch-mode item - ignoring.")
      (newline))))

;;  (statprof-reset 0 50000) ;; 20 times/sec
;;  (statprof-start)

  ;; Now the fun begins.
  (gnc:startup)

  (gnc:print-unstable-message)

  (if (null? gnc:*batch-mode-things-to-do*)
      ;; We're not in batch mode; we can go ahead and do the normal thing.
      (begin
        (gnc:hook-add-dangler gnc:*ui-shutdown-hook* gnc:ui-finish)
        (gnc:ui-init)
        (if (and
             (not (gnc:account-file-to-load))
             (not (string? (gnc:history-get-last)))
             (gnc:option-value
              (gnc:lookup-global-option "__new_user" "first_startup")))
            (gnc:new-user-dialog)
            (gnc:load-account-file))
        (gnc:start-ui-event-loop)
        (gnc:hook-remove-dangler gnc:*ui-shutdown-hook* gnc:ui-finish))

      ;; else: we're in batch mode.  Just do what the user said on the
      ;; command line
      (map handle-batch-mode-item (reverse gnc:*batch-mode-things-to-do*)))

  (gnc:shutdown 0))
