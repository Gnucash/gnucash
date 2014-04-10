(define-module (g-wrapped gw-kvp-spec)
  :use-module (g-wrap)
  :use-module (g-wrapped gw-glib-spec)
  :use-module (g-wrapped gw-engine-spec))

(let ((mod (gw:new-module "gw-kvp")))

  (define (standard-c-call-gen result func-call-code)
    (list (gw:result-get-c-name result) " = " func-call-code ";\n"))
  
  (define (add-standard-result-handlers! type c->scm-converter)
    (define (standard-pre-handler result)
      (let* ((ret-type-name (gw:result-get-proper-c-type-name result))
             (ret-var-name (gw:result-get-c-name result)))
        (list "{\n"
              "    " ret-type-name " " ret-var-name ";\n")))

    (gw:type-set-pre-call-result-ccodegen! type standard-pre-handler)
    
    (gw:type-set-post-call-result-ccodegen!
     type
     (lambda (result)
       (let* ((scm-name (gw:result-get-scm-name result))
              (c-name (gw:result-get-c-name result)))
         (list
          (c->scm-converter scm-name c-name)
          "  }\n")))))

  (gw:module-depends-on mod "gw-runtime")
  (gw:module-depends-on mod "gw-glib")
  (gw:module-depends-on mod "gw-engine")
  (gw:module-set-guile-module! mod '(g-wrapped gw-kvp))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <kvp_frame.h>\n"
      "#include <kvp-scm.h>\n"
      "#include <Transaction.h>\n")))
;;  (gw:module-set-init-ccodegen!
;;   mod
;;   (lambda (client-only?) 
;;     (if client-only? 
;;         '()
;;         (gw:inline-scheme '(use-modules (gnucash kvp))))))

  
  (gw:wrap-non-native-type mod '<gnc:kvp-frame*> 
                           "kvp_frame*" "const kvp_frame*")

  (let ((wt (gw:wrap-type mod '<gnc:kvp-value*>
                          "kvp_value*" "const kvp_value*")))
    
    (gw:type-set-scm-arg-type-test-ccodegen!
     wt
     (lambda (param)
       (list "gnc_kvp_value_ptr_p(" (gw:param-get-scm-name param) ")")))
    
    (gw:type-set-pre-call-arg-ccodegen!
     wt
     (lambda (param)
       (list (gw:param-get-c-name param) " = "
             (list "gnc_scm_to_kvp_value_ptr("
                   (gw:param-get-scm-name param) ")")
             ";\n")))
    
    (gw:type-set-call-ccodegen! wt standard-c-call-gen)

    (add-standard-result-handlers!
     wt
     (lambda (scm-name c-name)
       (let ((old-func (lambda (x) (list "gnc_kvp_value_ptr_to_scm(" x ")"))))
         (list scm-name " = " (old-func c-name) ";\n")))))

;;  (gw:wrap-function
;;   mod
;;   'gnc:split-get-slots
;;   '<gnc:kvp-frame*>
;;   "xaccSplitGetSlots"
;;   '((<gnc:Split*> s))
;;   "Get the split's slots.")

  
  (gw:wrap-function
   mod
   'gnc:transaction-get-slots
   '<gnc:kvp-frame*>
   "xaccTransGetSlots"
   '((<gnc:Transaction*> s))
   "Get the transaction's slots.")

  (gw:wrap-function
   mod
   'gnc:kvp-frame-set-slot
   '<gw:void>
   "kvp_frame_set_slot"
   '((<gnc:kvp-frame*> k) ((<gw:m-chars-caller-owned> gw:const) c)
     (<gnc:kvp-value*> v))
   "Sets the slot c in frame k to the value v")

  (gw:wrap-function
   mod
   'gnc:kvp-frame-get-slot
   '<gnc:kvp-value*>
   "kvp_frame_get_slot"
   '((<gnc:kvp-frame*> k) ((<gw:m-chars-caller-owned> gw:const) c))
   "Gets the slot c from frame k")
  
;;  (gw:wrap-function
;;   mod
;;   'gnc:account-get-slots
;;   '<gnc:kvp-frame*>
;;   "xaccAccountGetSlots"
;;   '((<gnc:Account*> s))
;;   "Get the account's slots.")
)
