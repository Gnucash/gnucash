
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
  (if (gnc:debugging?)
      (gnc:register-translatable-strings name documentation-string))
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


(define (gnc:extensions-menu-setup win)
  (define menu (gnc:make-menu "Extensions" (list "_Settings")))
  
  (define export-item
    (gnc:make-menu-item "Export data as text (Danger: Unfinished)"
                        "Export data as text."
                        (list "Extensions" "")
                        (lambda () (gnc:main-win-export-data-as-text win))))
  
  (define qif-item
    (gnc:make-menu-item "QIF File Import (Danger: Unfinished)"
                        "Import QIF File - Scripted in Guile."
                        (list "Extensions"
                              "Export data as text (Danger: Unfinished)")
                        (lambda () (gnc:extensions-qif-import win))))

  (define strings-item
    (gnc:make-menu-item
     "Save Translatable Strings"
     "Save strings that need to be translated"
     (list "Extensions" "")
     (lambda ()
       (let ((file-name (gnc:file-selection-dialog
                         "Select file to save strings in" "")))
         (if file-name (gnc:save-translatable-strings file-name))))))

  (gnc:add-extension menu)
  (gnc:add-extension export-item)
  (gnc:add-extension qif-item)

  (if (gnc:debugging?)
      (gnc:add-extension strings-item)))


(if (gnc:debugging?)
    (gnc:hook-add-dangler gnc:*main-window-opened-hook*
                          gnc:extensions-menu-setup))


;; Automatically pick accelerators for menu names
(define (gnc:new-menu-namer)

  (define letter-hash (make-hash-table 23))
  (define name-hash (make-hash-table 23))

  (define (add-name raw-name)
    (let* ((name (gnc:_ raw-name))
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

      (if (gnc:debugging?)
          (gnc:register-translatable-strings raw-name))

      (try-at-k 0)))

  (define (lookup name)
    (hash-ref name-hash name))

  (define (dispatch key)
    (case key
      ((add-name) add-name)
      ((lookup) lookup)))

  dispatch)
