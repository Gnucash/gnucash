;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main-window.scm : utilities for dealing with main window
;; Copyright 2001 Bill Gribble <grib@gnumatic.com>
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; book open and close hooks for mdi 
;; 
;; we need to save all the active report and acct tree info during
;; book close.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:main-window-save-state session)
  (let* ((book-url (gnc:session-get-url session))
	 (conf-file-name (gnc:html-encode-string book-url))
         (book-path #f))

    (if conf-file-name
        (let ((book-path (gnc:build-book-path conf-file-name)))
          (with-output-to-port (open-output-file book-path)
            (lambda ()
              (hash-fold 
               (lambda (k v p)
                 (if (gnc:report-needs-save? v)
                     (display (gnc:report-generate-restore-forms v))))
               #t *gnc:_reports_*)

              (force-output)))
	  ))))

(define (gnc:main-window-book-close-handler session)
  (let ((dead-reports '()))
    ;; get a list of the reports we'll be needing to nuke     
    (hash-fold 
     (lambda (k v p)
       (set! dead-reports (cons k dead-reports)) #t) #t *gnc:_reports_*)

    ;; actually remove them (if they're being displayed, the
    ;; window's reference will keep the report alive until the
    ;; window is destroyed, but find-report will fail)
    (for-each 
     (lambda (dr)
       (hash-remove! *gnc:_reports_* dr))
     dead-reports)))

(define (gnc:main-window-book-open-handler session)
  (define (try-load file-suffix)
    (let ((file (gnc:build-book-path file-suffix)))
      ;; make sure the books directory is there 
      (if (access? file F_OK)
          (if (not (false-if-exception (primitive-load file)))
              (begin
                (gnc:warn "failure loading " file)
                #f))
          #f)))
  (let* ((book-url (gnc:session-get-url session))
	 (conf-file-name (gnc:html-encode-string book-url))
	 (dead-reports '()))
    (if conf-file-name 
        (try-load conf-file-name))

    ;; the reports have only been created at this point; create their ui component.
    (hash-fold (lambda (key val prior-result)
                (gnc:main-window-open-report (gnc:report-id val)
                                             #f ;; =window: #f/null => open in first window
                                             ))
               #t *gnc:_reports_*)
    ))

(define (gnc:main-window-properties-cb)
  (let* ((book (gnc:get-current-book))
	 (slots (gnc:book-get-slots book)))

    (define (changed_cb)
      (gnc:book-kvp-changed book))
			    
    (gnc:kvp-option-dialog gnc:id-book
			   slots (_ "Book Options")
			   changed_cb)))

(gnc:hook-remove-dangler gnc:*book-closed-hook* 
                         gnc:main-window-book-close-handler)
(gnc:hook-add-dangler gnc:*book-closed-hook* 
                      gnc:main-window-book-close-handler)
