;;; -*-scheme-*-

;(debug-enable 'backtrace)
;(debug-enable 'debug)
;(read-enable 'positions)

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-business-core-spec)
  :use-module (g-wrap))

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrap gw-wct-spec))

(let ((ws (gw:new-wrapset "gw-business-core")))
  
  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")

  (gw:wrapset-depends-on ws "gw-engine")
  
  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-business-core))
  
  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <gncAddress.h>\n"
      "#include <gncCustomer.h>\n"
      "#include <gncEmployee.h>\n"
      "#include <gncEntry.h>\n"
      "#include <gncInvoice.h>\n"
      "#include <gncJob.h>\n"
      "#include <gncOrder.h>\n"
      "#include <gncOwner.h>\n"
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
  (gw:wrap-as-wct ws '<gnc:GncOwner*> "GncOwner*" "const GncOwner*")
  (gw:wrap-as-wct ws '<gnc:GncVendor*> "GncVendor*" "const GncVendor*")

  (let ((wt (gw:wrap-enumeration ws '<gnc:GncOwnerType> "GncOwnerType")))
    (gw:enum-add-value! wt "GNC_OWNER_NONE" 'gnc-owner-none)
    (gw:enum-add-value! wt "GNC_OWNER_UNDEFINED" 'gnc-owner-undefined)
    (gw:enum-add-value! wt "GNC_OWNER_CUSTOMER" 'gnc-owner-customer)
    (gw:enum-add-value! wt "GNC_OWNER_JOB" 'gnc-owner-job)
    (gw:enum-add-value! wt "GNC_OWNER_VENDOR" 'gnc-owner-vendor)
    #t)

  ;; gncAddress.h

  ;; gncCustomer.h

  ;; gncEmployee.h

  ;; gncEntry.h

  ;; gncInvoice.h

  ;; gncJob.h

  ;; gncOrder.h

  ;; gncOwner.h
  (gw:wrap-function
   ws
   'gnc:owner-create
   '<gnc:GncOwner*>
   "gncOwnerCreate"
   '()
   "Create a GncOwner object")

  (gw:wrap-function
   ws
   'gnc:owner-destroy
   '<gw:void>
   "gncOwnerDestroy"
   '((<gnc:GncOwner*> owner))
   "Destroy a GncOwner object")

  (gw:wrap-function
   ws
   'gnc:owner-init-customer
   '<gw:void>
   "gncOwnerInitCustomer"
   '((<gnc:GncOwner*> owner) (<gnc:GncCustomer*> customer))
   "Initialize an owner to hold a Customer.  The Customer may be NULL.")

  (gw:wrap-function
   ws
   'gnc:owner-init-job
   '<gw:void>
   "gncOwnerInitJob"
   '((<gnc:GncOwner*> owner) (<gnc:GncJob*> job))
   "Initialize an owner to hold a Job.  The Job may be NULL.")

  (gw:wrap-function
   ws
   'gnc:owner-init-vendor
   '<gw:void>
   "gncOwnerInitVendor"
   '((<gnc:GncOwner*> owner) (<gnc:GncVendor*> vendor))
   "Initialize an owner to hold a Vendor.  The Vendor may be NULL.")

  (gw:wrap-function
   ws
   'gnc:owner-get-type
   '<gnc:GncOwnerType>
   "gncOwnerGetType"
   '((<gnc:GncOwner*> owner))
   "Return the type of this owner.")

  (gw:wrap-function
   ws
   'gnc:owner-get-customer
   '<gnc:GncCustomer*>
   "gncOwnerGetCustomer"
   '((<gnc:GncOwner*> owner))
   "Return the customer of this owner.")

  (gw:wrap-function
   ws
   'gnc:owner-get-job
   '<gnc:GncJob*>
   "gncOwnerGetJob"
   '((<gnc:GncOwner*> owner))
   "Return the job of this owner.")

  (gw:wrap-function
   ws
   'gnc:owner-get-vendor
   '<gnc:GncVendor*>
   "gncOwnerGetVendor"
   '((<gnc:GncOwner*> owner))
   "Return the vendor of this owner.")

  (gw:wrap-function
   ws
   'gnc:owner-equal
   '<gw:bool>
   "gncOwnerEqual"
   '((<gnc:GncOwner*> owner1) (<gnc:GncOwner*> owner2))
   "Compare owner1 and owner2 and return if they are equal")

  ;; gncVendor.h

)
