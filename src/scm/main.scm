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

;; Load the srfis (eventually, we should see where these are needed
;; and only have the use-modules statements in those files).
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))
(use-modules (gnucash gnc-module))
(use-modules (ice-9 slib))
(require 'printf)

;;(use-modules (ice-9 statprof))

;; A list of things to do when in batch mode after the initial
;; startup.  List items may be strings, in which case they're read and
;; evaluated or procedures, in which case they're just executed.
;; The items will be done in reverse order.
(define gnc:*batch-mode-things-to-do* '())

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

  ;; initialize the gnucash module system 
  (gnc:module-system-init)
  
  ;; SUPER UGLY HACK -- this should go away when I come back for the
  ;; second cleanup pass...
  (let ((original-module (current-module))
        (bootstrap (resolve-module '(gnucash bootstrap))))
    
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
                 (eval next-form (resolve-module '(gnucash bootstrap)))
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
