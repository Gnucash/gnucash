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

;; A list of things to do when in batch mode after the initial
;; startup.  List items may be strings, in wich case they're read and
;; evaluated or procedures, in which case they're just executed.
;; The items will be done in reverse order.
(define gnc:*batch-mode-things-to-do* '())

(define (gnc:startup)
  (gnc:debug "starting up.")
  (if (not (gnc:handle-command-line-args))
      (gnc:shutdown 1))

  (gnc:setup-debugging)

  ;; Load the srfis
  (gnc:load "srfi/srfi-8.guile.scm")
  (gnc:load "srfi/srfi-1.unclear.scm")
  (gnc:load "srfi/srfi-1.r5rs.scm")

  ;; Now we can load a bunch of files.

  (gnc:depend "doc.scm")
  (gnc:depend "extensions.scm")
  (gnc:depend "text-export.scm")
  (gnc:depend "report.scm")
  (gnc:depend "commodity-import.scm")
  (gnc:depend "report/report-list.scm")
  (gnc:depend "qif-import/qif-import.scm")
  (gnc:depend "printing/print-check.scm")

  ;; Load the system configs
  (if (not (gnc:load-system-config-if-needed))
      (gnc:shutdown 1))

  ;; Load the user configs
  (gnc:load-user-config-if-needed)

  ;; Clear the change flags caused by loading the configs
  (gnc:global-options-clear-changes)

  (gnc:hook-run-danglers gnc:*startup-hook*)

  ;; Initialize the C side options code. Must come after the scheme
  ;; options are loaded.
  (gnc:c-options-init)

  ;; Initialize the expresion parser. Must come after the C side
  ;; options initialization.
  (gnc:exp-parser-init)

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
	     (begin
               (gnc:ui-destroy-all-subwindows)
               (if (gnc:file-query-save)
                   (begin
                     (gnc:hook-run-danglers gnc:*ui-shutdown-hook*)
                     (gnc:ui-shutdown))))))
        (else
	 (gnc:ui-destroy)
	 (gnc:hook-run-danglers gnc:*shutdown-hook*)
         (gnc:engine-shutdown)
	 (exit exit-status))))

(define (gnc:ui-finish)
  (gnc:debug "UI Shutdown hook.")
  (gnc:file-quit))

(define (gnc:load-account-file)
  (let ((ok (not (gnc:config-var-value-get gnc:*arg-no-file*)))
        (file (if (pair? gnc:*command-line-remaining*)
                  (car gnc:*command-line-remaining*)
                  (gnc:history-get-last))))
    (if (and ok (string? file))
        (gnc:ui-open-file file))))

(define (gnc:main)

  ;; Now the fun begins.
  (gnc:startup)

  (if (not (= (gnc:lowlev-app-init) 0))
      (gnc:shutdown 0))

  ;; add a hook to shut down the expression parser
  (gnc:hook-add-dangler gnc:*shutdown-hook* gnc:exp-parser-shutdown)

  ;; add a hook to save the user configs on shutdown
  (gnc:hook-add-dangler gnc:*shutdown-hook* gnc:save-global-options)

  ;; add a hook to shut down the C side options code
  (gnc:hook-add-dangler gnc:*shutdown-hook* gnc:c-options-shutdown)

  (if (null? gnc:*batch-mode-things-to-do*)
      ;; We're not in batch mode; we can go ahead and do the normal thing.
      (begin
        ;; (gnc:load-account-file)
        (gnc:hook-add-dangler gnc:*ui-shutdown-hook* gnc:ui-finish)
        (gnc:ui-main)
        (gnc:hook-remove-dangler gnc:*ui-shutdown-hook* gnc:ui-finish))
      
      ;; else: we're in batch mode.  Just do what the user said on the
      ;; command line
      (map handle-batch-mode-item (reverse gnc:*batch-mode-things-to-do*)))
  
  (gnc:shutdown 0))
