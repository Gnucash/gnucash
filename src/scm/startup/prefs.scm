;;;; Preferences...

(use-modules (ice-9 slib))
(require 'sort)

;; (define gnc:*double-entry-restriction*
;;   (gnc:make-config-var
;;    "Determines how the splits in a transaction will be balanced. 
;;  The following values have significance:
;; 
;;    #f        anything goes
;; 
;;    'force    The sum of all splits in a transaction will be
;;              forced to be zero, even if this requires the
;;              creation of additional splits.  Note that a split
;;              whose value is zero (e.g. a stock price) can exist
;;              by itself. Otherwise, all splits must come in at 
;;              least pairs.
;; 
;;    'collect  splits without parents will be forced into a
;;              lost & found account.  (Not implemented)"
;;    (lambda (var value)
;;      (cond
;;       ((eq? value #f)
;;        (_gnc_set_force_double_entry_ 0)
;;        (list value))
;;       ((eq? value 'force)
;;        (_gnc_set_force_double_entry_ 1)
;;        (list value))
;;       ((eq? value 'collect)
;;        (gnc:warn
;;         "gnc:*double-entry-restriction* -- 'collect not supported yet.  "
;;         "Ignoring.")
;;        #f)
;;       (else
;;        (gnc:warn
;;         "gnc:*double-entry-restriction* -- " value " not supported.  Ignoring.")
;;        #f)))
;;    eq?
;;    #f))

;; We'd rather use a hash table for this, but until hash-for-each or
;; hash-keys is generally available, we can't...
(define gnc_:*options-dialog-entries* '())

;; This will be an alist
;;   (k v) -> (section-name list-of-option-items)

;; For now all the setters need to be idempotent.  We may call them
;; more than once per value change.  This is because of the way we
;; handle cancel and apply.

(define (gnc:make-configuration-option
         section
         name
         sort-tag
         type
         documentation-string
         getter
         setter
         default-getter
         generate-restore-form
         ui-value-validator)
  (vector section
          name
          sort-tag
          type
          documentation-string
          getter
          setter
          default-getter
          generate-restore-form
          #f
          ui-value-validator))

(define (gnc:configuration-option-section option)
  (vector-ref option 0))
(define (gnc:configuration-option-name option)
  (vector-ref option 1))
(define (gnc:configuration-option-sort-tag option)
  (vector-ref option 2))
(define (gnc:configuration-option-type option)
  (vector-ref option 3))
(define (gnc:configuration-option-documentation option)
  (vector-ref option 4))
(define (gnc:configuration-option-getter option)
  (vector-ref option 5))
(define (gnc:configuration-option-setter option)
  (vector-ref option 6))
(define (gnc:configuration-option-default-getter option)
  (vector-ref option 7))
(define (gnc:configuration-option-generate-restore-form option)
  (vector-ref option 8))

(define (gnc:configuration-option-widget-get option)
  (vector-ref option 9))
(define (gnc:configuration-option-widget-set! option widget)
  (vector-set! option 9 widget))

;; Validation func should return (#t value) on success, and
;; (#f "failure-message") on failure.
(define (gnc:configuration-option-ui-value-validator option)
  (vector-ref option 10))


(define (gnc:register-configuration-option new-item)
  
  (let* ((section (gnc:configuration-option-section new-item))
         (existing-entry (assoc-ref gnc_:*options-dialog-entries* section)))
    (if existing-entry
        (set! gnc_:*options-dialog-entries*
              (assoc-set! gnc_:*options-dialog-entries*
                          section 
                          (cons new-item existing-entry)))
        (set! gnc_:*options-dialog-entries*
              (assoc-set! gnc_:*options-dialog-entries*
                          section 
                          (list new-item))))))

;; Cancel checkpoint actions.

(define (gnc:options-dialog-clear-cancel-actions) #f)
(define (gnc:options-dialog-apply-cancel-actions) #f)
(define (gnc:options-dialog-add-cancel-action action) #f)

(let ((cancel-actions '()))
  (set! gnc:options-dialog-clear-cancel-actions
        (lambda () (set! cancel-actions '())))
  (set! gnc:options-dialog-apply-cancel-actions
        (lambda ()
          (for-each (lambda (a) (a)) (reverse cancel-actions))))
  (set! gnc:options-dialog-add-cancel-action
        (lambda (action)
          (set! cancel-actions (cons action cancel-actions)))))

(define (gnc:options-dialog-cancel-clicked)
  (gnc:options-dialog-apply-cancel-actions))

;; Apply checkpoint actions.

(define (gnc:options-dialog-clear-ok-actions) #f)
(define (gnc:options-dialog-get-ok-actions) #f)
(define (gnc:options-dialog-add-ok-action action) #f)

(let ((ok-actions '()))
  (set! gnc:options-dialog-clear-ok-actions
        (lambda () (set! ok-actions '())))
  (set! gnc:options-dialog-get-ok-actions
        (lambda ()
          ok-actions))
  (set! gnc:options-dialog-add-ok-action
        (lambda (action)
          (set! ok-actions (cons action ok-actions)))))

(define (gnc:options-dialog-ok-clicked)
  (let ((actions (reverse (gnc:options-dialog-get-ok-actions))))
    (let execute-actions ((remainder actions))
      (cond ((null? remainder) #t)
            (else (if ((car remainder))
                      (execute-actions (cdr remainder))
                      #f))))))

(define (gnc_warning_dialog message)
  (gnc:warn message)
  (gnc:warn "This function needs to be replaced by a real UI."))

(define (gnc:options-dialog-item-apply-new-ui-value item)
  (let ((current-ui-value (_gnc_options_dialog_item_get_ui_value_ item))
        (validation-func (gnc:configuration-option-ui-value-validator item))
        (verification-result #f))
    
    (if validation-func
        (set! verification-result (validation-func current-ui-value))
        (set! verification-result (list current-ui-value)))
    
    (if (car verification-result)
        (begin
          ;; if it's OK then update item, refresh UI, and return #t
          ((gnc:configuration-option-setter item) (cadr verification-result))
          (_gnc_options_dialog_item_refresh_ui_ item)
          #t)
        (begin
          (gnc_warning_dialog (cadr verification-result))
          #f))))

(define (gnc_:insert-options-dialog-item gnome-widget configuration-item)

  ;; Set things up so that we can revert to the current value if the
  ;; user hits cancel (elegant method, no?).
  (gnc:options-dialog-add-cancel-action
   (let ((current-value ((gnc:configuration-option-getter configuration-item)))
         (setter (gnc:configuration-option-setter configuration-item)))
     (lambda ()
       (setter current-value))))

  (gnc:options-dialog-add-ok-action
   (lambda ()
     (gnc:options-dialog-item-apply-new-ui-value configuration-item)))

  (_gnc_options_dialog_add_item_ gnome-widget configuration-item))


(define (gnc_:build-options-dialog-page section-info)
  ;; section-info is a pair (section-name . list-of-options)
  (let ((gtk-page-widget (_gnc_options_dialog_add_page_ (car section-info)))
        (sorted-section-items
         (sort (cdr section-info)
               (lambda (x y)
                 (string<? (gnc:configuration-option-sort-tag x)
                           (gnc:configuration-option-sort-tag y))))))
    (for-each (lambda (item)
                (gnc_:insert-options-dialog-item gtk-page-widget item))
              sorted-section-items)))

(define (gnc_:build-options-dialog)
  (for-each gnc_:build-options-dialog-page
            (sort gnc_:*options-dialog-entries*
                  (lambda (x y)
                    (string<? (car x)
                              (car y))))))


(define (set-background-color! c) #f)
(define (get-background-color) #f)
(define (default-background-color) "grey")

(let ((color (default-background-color)))
  (set! set-background-color! (lambda (c) (set! color c)))
  (set! get-background-color (lambda () color)))

(gnc:register-configuration-option
 (gnc:make-configuration-option "Appearance"
                                "Default background color"
                                "50-background-color"
                                'string
                                "Set the default background color."
                                get-background-color
                                set-background-color!
                                default-background-color
                                #f
                                #f))
(gnc:register-configuration-option
 (gnc:make-configuration-option "Appearance"
                                "foo2"
                                "50-foo2"
                                'boolean
                                "foo2 something"
                                (lambda ()
                                  (display "getting\n")
                                  #f)
                                (lambda (x)
                                  (display "setting\n")
                                  #f)
                                #f
                                #f
                                #f))
(gnc:register-configuration-option
 (gnc:make-configuration-option "Security"
                                "foo"
                                "50-foo"
                                'string
                                "foo something"
                                (lambda ()
                                  (display "getting\n")
                                  #f)
                                (lambda (x)
                                  (display "setting\n")
                                  #f)
                                #f
                                #f
                                #f))
(gnc:register-configuration-option
 (gnc:make-configuration-option "Register"
                                "foo"
                                "50-foo"
                                'boolean
                                "foo something"
                                (lambda ()
                                  (display "getting\n")
                                  #f)
                                (lambda (x)
                                  (display "setting\n")
                                  #f)
                                #f
                                #f
                                #f))

(for-each
 (lambda (x) (display x) (newline))
 gnc_:*options-dialog-entries*)
(newline)
; =======
; ;; Preferences...

; (define gnc:*double-entry-restriction*
;   (gnc:make-config-var
;    "Determines how the splits in a transaction will be balanced. 
;  The following values have significance:

;    #f        anything goes

;    'force    The sum of all splits in a transaction will be
;              forced to be zero, even if this requires the
;              creation of additional splits.  Note that a split
;              whose value is zero (e.g. a stock price) can exist
;              by itself. Otherwise, all splits must come in at 
;              least pairs.

;    'collect  splits without parents will be forced into a
;              lost & found account.  (Not implemented)"
;    (lambda (var value)
;      (cond
;       ((eq? value #f)
;        (xaccConfigSetForceDoubleEntry 0)
;        (list value))
;       ((eq? value 'force)
;        (xaccConfigSetForceDoubleEntry 1)
;        (list value))
;       ((eq? value 'collect)
;        (gnc:warn
;         "gnc:*double-entry-restriction* -- 'collect not supported yet.  "
;         "Ignoring.")
;        #f)
;       (else
;        (gnc:warn
;         "gnc:*double-entry-restriction* -- " value " not supported.  Ignoring.")
;        #f)))
;    eq?
;    #f))

(define gnc:*arg-show-usage*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*arg-show-help*
  (gnc:make-config-var
   "Generate an argument summary."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*debugging?*
  (gnc:make-config-var
   "Enable debugging code."
   (lambda (var value) (if (boolean? value) (list value) #f))
   eq?
   #f))

(define gnc:*startup-dir*
  (gnc:make-config-var
   "Location of initial lowest level scheme startup files."
   (lambda (var value)
     ;; You can't change the startup dir from here.  It's considered
     ;; hard-coded once known -- see startup/init.scm.
     #f)
   string=?
   gnc:_startup-dir-default_))

(define gnc:*config-dir*
  (gnc:make-config-var
   "Configuration directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_config-dir-default_))

(define gnc:*share-dir*
  (gnc:make-config-var
   "Shared files directory."
   (lambda (var value) (if (string? value) (list value) #f))
   string=?
   gnc:_share-dir-default_))

(define gnc:*load-path*
  (gnc:make-config-var
   "A list of strings indicating the load path for (gnc:load name).
Any path element enclosed in parentheses will automatically be
expanded to that directory and all its subdirectories whenever this
variable is modified.  The symbol element default will expand to the default directory.  i.e. (gnc:config-var-value-set! gnc:*load-path* '(\"/my/dir/\" default))"
   (lambda (var value)
     (if (not (list? value))
         #f
         (let ((result (gnc:_load-path-update_ var value)))
           (if (list? result)
               (list result)
               #f))))
   equal?
   (list 
    (string-append "(" (getenv "HOME") "/.gnucash/scm)")
    (string-append "(" gnc:_share-dir-default_ "/scm)"))))

(define gnc:*doc-path*
  (gnc:make-config-var
   "A list of strings indicating where to look for html and parsed-html files
Any path element enclosed in parentheses will automatically be
expanded to that directory and all its subdirectories whenever this
variable is modified.  The symbol element default will expand to the
default directory.  i.e. (gnc:config-var-value-set! gnc:*doc-path*
'(\"/my/dir/\" default))"
   (lambda (var value)
     (if (not (list? value))
         #f
         (let ((result (gnc:_doc-path-update_ var value)))
           (if (list? result)
               (list result)
               #f))))
   equal?
   (list
    (string-append "(" (getenv "HOME") "/.gnucash/doc)")    
    (string-append "(" gnc:_share-dir-default_ "/Docs)")
    (string-append "(" gnc:_share-dir-default_ "/Reports)"))))
