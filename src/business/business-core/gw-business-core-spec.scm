;;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-business-core-spec)
  :use-module (g-wrap))

(use-modules (g-wrapped gw-engine-spec))

(let ((mod (gw:new-module "gw-business-core")))
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
  (gw:module-depends-on mod "gw-engine")

  (gw:module-set-guile-module! mod '(g-wrapped gw-business-core))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <gncBusiness.h>\n"
      "#include <gncAddress.h>\n"
      "#include <gncCustomer.h>\n"
      "#include <gncEmployee.h>\n"
      "#include <gncEntry.h>\n"
      "#include <gncInvoice.h>\n"
      "#include <gncJob.h>\n"
      "#include <gncOrder.h>\n"
      "#include <gncVendor.h>\n"
      )))

  (gw:module-set-init-ccodegen!
   mod
   (lambda (client-only?) 
     (if client-only? 
         '()
         (gw:inline-scheme '(use-modules (gnucash business-core))))))

  ;; The core Business Object

  (gw:wrap-non-native-type mod '<gnc:GncBusiness*> "GncBusiness*"
			   "const GncBusiness*")

  ;; The core Business Object Types
  ;; XXX: Need to add lists of all of these!

  (gw:wrap-non-native-type mod '<gnc:GncAddress*> "GncAddress*"
			   "const GncAddress*")
  (gw:wrap-non-native-type mod '<gnc:GncCustomer*> "GncCustomer*"
			   "const GncCustomer*")
  (gw:wrap-non-native-type mod '<gnc:GncEmployee*> "GncEmployee*"
			   "const GncEmployee*")
  (gw:wrap-non-native-type mod '<gnc:GncEntry*> "GncEntry*"
			   "const GncEntry*")
  (gw:wrap-non-native-type mod '<gnc:GncInvoice*> "GncInvoice*"
			   "const GncInvoice*")
  (gw:wrap-non-native-type mod '<gnc:GncJob*> "GncJob*"
			   "const GncJob*")
  (gw:wrap-non-native-type mod '<gnc:GncOrder*> "GncOrder*"
			   "const GncOrder*")
  (gw:wrap-non-native-type mod '<gnc:GncVendor*> "GncVendor*"
			   "const GncVendor*")

  ;;
  ;; gncBusiness.h
  ;;

  (gw:wrap-function
   mod
   'gnc:business-create
   '<gnc:GncBusiness*>
   "gncBusinessCreate"
   '((<gnc:Book*> book))
   "Return a newly-created GncBusiness state object.")

  (gw:wrap-function
   mod
   'gnc:business-destroy
   '<gw:void>
   "gncBusinessDestroy"
   '((<gnc:GncBusiness*> business))
   "Destroy a GncBusiness object")

  ;; gncAddress.h

  ;; gncCustomer.h

  ;; gncEmployee.h

  ;; gncEntry.h

  ;; gncInvoice.h

  ;; gncJob.h

  ;; gncOrder.h

  ;; gncVendor.h

)
