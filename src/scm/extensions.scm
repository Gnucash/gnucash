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

(gnc:support "extensions.scm")

(define (gnc:make-extension
	 ;; The type of extension item, either 'menu, 'menu-item, or 'separator
         type
         ;; The name of the extension in the menu
         name
         ;; The tooltip
         documentation-string
         ;; A list of names indicating the menus under which this item is
         ;; located. The last item indicates the item *after* which this
         ;; extension will go.
         path
         ;; The script to call when the menu item is selected
         script)
  (vector type
          name
          documentation-string
          path
          script))

(define (gnc:extension-type extension)
  (vector-ref extension 0))
(define (gnc:extension-name extension)
  (vector-ref extension 1))
(define (gnc:extension-documentation extension)
  (vector-ref extension 2))
(define (gnc:extension-path extension)
  (vector-ref extension 3))
(define (gnc:extension-script extension)
  (vector-ref extension 4))

(define (gnc:make-menu-item name documentation-string path script)
  (gnc:make-extension 'menu-item name documentation-string path script))

(define (gnc:make-menu name path)
  (gnc:make-extension 'menu name #f path #f))

(define (gnc:make-separator path)
  (gnc:make-extension 'separator #f #f path #f))


(define (gnc:extensions-menu-setup)
  (define menu (gnc:make-menu "Extensions" (list "_Settings")))
  
  (define schedxact-editor-item
    (gnc:make-menu-item (N_ "SchedXact: Editor")
			(N_ "Editor of Scheduled Transactions")
			(list "Extensions" "")
			(lambda () (gnc:sx-editor)))
    )
  (define schedxact-old-slr-item
    (gnc:make-menu-item (N_ "SchedXact: [Old] Since Last Run")
			(N_ "since last run dialog")
			(list "Extensions" "")
			(lambda () (gnc:sx-since-last-run)))
    )
  (define schedxact-slr-item
    (gnc:make-menu-item (N_ "SchedXact: [New] Since Last Run")
                        (N_ "since last run dialog")
                        (list "Extensions" "" )
                        (lambda () (gnc:sx-sincelast-create ))))
  (define progress-item
    (gnc:make-menu-item (N_ "Test progress dialog")
                        (N_ "Test progress dialog")
                        (list "Extensions" "")
                        (lambda ()
                          (let ((dialog (gnc:progress-dialog-new #f #f))
                                (canceled #f))
                            (gnc:progress-dialog-set-activity-mode dialog #t)
                            (gnc:progress-dialog-set-heading dialog #f)
                            (gnc:progress-dialog-set-cancel-scm-func
                             dialog
                             (lambda ()
                               (display "User canceled.") (newline)
                               (set! canceled #t)
                               #t))
                            (let loop ((value 0.0))
                              (gnc:progress-dialog-set-value dialog value)
                              (sleep 1)
                              (if (and (not canceled) (< value 90.0))
                                  (loop (+ value 5.0))))
                            (gnc:progress-dialog-finish dialog)
                            (gnc:progress-dialog-destroy dialog)))))

  (gnc:add-extension menu)
;  (gnc:add-extension export-item)

;; NOTE: this is the inverse order from how you may want them to
;; appear in the menu [prepending to some list]...

  (gnc:add-extension progress-item)
  (gnc:add-extension schedxact-old-slr-item)
  (gnc:add-extension schedxact-slr-item)
  (gnc:add-extension schedxact-editor-item)
)

(if (gnc:debugging?)
    (gnc:hook-add-dangler gnc:*ui-startup-hook*
                          gnc:extensions-menu-setup))


;; Automatically pick accelerators for menu names
(define (gnc:new-menu-namer)

  (define letter-hash (make-hash-table 23))
  (define name-hash (make-hash-table 23))

  (define (add-name raw-name)
    (let* ((name (_ raw-name))
           (length (string-length name)))

      (define (try-at-k k)
        (if (>= k length)
            (begin
              (hash-set! name-hash raw-name name)
              name)
            (let* ((char (char-upcase (string-ref name k)))
                   (used (hash-ref letter-hash char)))
              (if (not used)
                  (let ((new-name (string-append
                                   (substring name 0 k)
                                   "_"
                                   (substring name k length))))
                    (hash-set! letter-hash char #t)
                    (hash-set! name-hash raw-name new-name)
                    new-name)
                  (try-at-k (+ k 1))))))

      (try-at-k 0)))

  (define (lookup name)
    (hash-ref name-hash name))

  (define (dispatch key)
    (case key
      ((add-name) add-name)
      ((lookup) lookup)))

  dispatch)
