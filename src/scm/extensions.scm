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

(gnc:module-load "gnucash/business-gnome" 0)

(define gnc:extensions-temp-business #f)

(define (gnc:extensions-get-business)
  (if gnc:extensions-temp-business
      gnc:extensions-temp-business
      (begin
	(set! gnc:extensions-temp-business (gnc:business-create
					    (gnc:get-current-book)))
	gnc:extensions-temp-business)))

(define (gnc:extensions-menu-setup)
  (define menu (gnc:make-menu "Extensions" (list "_Settings")))


  (define new-job-item
    (gnc:make-menu-item (N_ "Test New Job Dialog")
			(N_ "Test New Job Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:job-new #f (gnc:extensions-get-business)
				       #f))))

  (define select-job-item
    (gnc:make-menu-item (N_ "Test Job Selection Dialog")
			(N_ "Test Job Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:job-select #f (gnc:extensions-get-business)
				       #f #f))))

  (define new-vendor-item
    (gnc:make-menu-item (N_ "Test New Vendor Dialog")
			(N_ "Test New Vendor Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:vendor-new #f (gnc:extensions-get-business)))))


  (define select-vendor-item
    (gnc:make-menu-item (N_ "Test Vendor Selection Dialog")
			(N_ "Test Vendor Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:vendor-select (gnc:extensions-get-business)
				       #f #f))))

  (define new-employee-item
    (gnc:make-menu-item (N_ "Test New Employee Dialog")
			(N_ "Test New Employee Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:employee-new #f (gnc:extensions-get-business)))))


  (define select-employee-item
    (gnc:make-menu-item (N_ "Test Employee Selection Dialog")
			(N_ "Test Employee Selection Dialog")
			(list "Extensions" "")
			(lambda ()
			  (gnc:employee-select (gnc:extensions-get-business)
				       #f #f))))

  (gnc:add-extension menu)
  (gnc:add-extension select-employee-item)
  (gnc:add-extension new-employee-item)
  (gnc:add-extension select-vendor-item)
  (gnc:add-extension new-vendor-item)
  (gnc:add-extension select-job-item)
  (gnc:add-extension new-job-item)
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
