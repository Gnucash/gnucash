;;; -*-scheme-*-

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-business-core-spec)
  :use-module (g-wrap))

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(use-modules (g-wrapped gw-engine-spec))

(let ((ws (gw:new-wrapset "gw-business-core")))
  
  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")

  (gw:wrapset-depends-on ws "gw-engine")
  
  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-business-core))
  
  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <gncBusiness.h>\n"
      "#include <gncAddress.h>\n"
      "#include <gncCustomer.h>\n"
      "#include <gncEmployee.h>\n"
      "#include <gncEntry.h>\n"
      "#include <gncInvoice.h>\n"
      "#include <gncJob.h>\n"
      "#include <gncOrder.h>\n"
      "#include <gncVendor.h>\n")))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var)
     (if client-wrapset
         '()
         (gw:inline-scheme '(use-modules (gnucash business-core))))))
  
  ;; The core Business Object Types
  ;; XXX: Need to add lists of all of these!

  (gw:wrap-as-wct ws '<gnc:GncAddress*> "GncAddress*" "const GncAddress*")
  (gw:wrap-as-wct ws '<gnc:GncCustomer*> "GncCustomer*" "const GncCustomer*")
  (gw:wrap-as-wct ws '<gnc:GncEmployee*> "GncEmployee*" "const GncEmployee*")
  (gw:wrap-as-wct ws '<gnc:GncEntry*> "GncEntry*" "const GncEntry*")
  (gw:wrap-as-wct ws '<gnc:GncInvoice*> "GncInvoice*" "const GncInvoice*")
  (gw:wrap-as-wct ws '<gnc:GncJob*> "GncJob*" "const GncJob*")
  (gw:wrap-as-wct ws '<gnc:GncOrder*> "GncOrder*" "const GncOrder*")
  (gw:wrap-as-wct ws '<gnc:GncVendor*> "GncVendor*" "const GncVendor*")

  ;;
  ;; gncBusiness.h
  ;;

  (gw:wrap-function
   ws
   'gnc:business-create-book
   '<gw:void>
   "gncBusinessCreateBook"
   '((<gnc:Book*> book))
   "Create the Business data tables in the book")

  ;; gncAddress.h

  ;; gncCustomer.h

  ;; gncEmployee.h

  ;; gncEntry.h

  ;; gncInvoice.h

  ;; gncJob.h

  ;; gncOrder.h

  ;; gncVendor.h

)
