;;; -*-scheme-*-

;(debug-enable 'backtrace)
;(debug-enable 'debug)
;(read-enable 'positions)

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-business-gnome-spec)
  :use-module (g-wrap))

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))

(use-modules (g-wrapped gw-business-core-spec))
(use-modules (g-wrapped gw-gnome-utils-spec))

(let ((ws (gw:new-wrapset "gw-business-gnome")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-depends-on ws "gw-business-core")
  (gw:wrapset-depends-on ws "gw-engine")
  (gw:wrapset-depends-on ws "gw-gnome-utils")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-business-gnome))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <dialog-customer.h>\n"
      "#include <dialog-employee.h>\n"
      "#include <dialog-invoice.h>\n"
      "#include <dialog-job.h>\n"
      "#include <dialog-job-select.h>\n"
      "#include <dialog-order.h>\n"
      "#include <dialog-vendor.h>\n"
      )))

  (gw:wrapset-add-cs-initializers!
   ws
   (lambda (wrapset client-wrapset status-var) 
     (if client-wrapset
         '()
         (gw:inline-scheme '(use-modules (gnucash business-gnome))))))
  
  ;;
  ;; dialog-customer.h
  ;;

  (gw:wrap-function
   ws
   'gnc:customer-new
   '<gnc:GncCustomer*>
   "gnc_customer_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncCustomer.  Parent may be NULL.")

  (gw:wrap-function
   ws
   'gnc:customer-edit
   '<gw:void>
   "gnc_customer_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncCustomer*> customer))
   "Dialog: Edit a GncCustomer.  Parent may be NULL.")


  (gw:wrap-function
   ws
   'gnc:customer-find
   '<gw:void>
   "gnc_customer_find"
   '((<gnc:UIWidget> parent) (<gnc:GncCustomer*> start_selection)
     (<gnc:Book*> book) )
   "Dialog: Find a GncCustomer.  Parent and start_selection may be NULL.")

  ;;
  ;; dialog-employee.h
  ;;

  (gw:wrap-function
   ws
   'gnc:employee-new
   '<gnc:GncEmployee*>
   "gnc_employee_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncEmployee.  Parent may be NULL.")

  (gw:wrap-function
   ws
   'gnc:employee-edit
   '<gw:void>
   "gnc_employee_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncEmployee*> employee))
   "Dialog: Edit a GncEmployee.  Parent may be NULL.")


  (gw:wrap-function
   ws
   'gnc:employee-find
   '<gw:void>
   "gnc_employee_find"
   '((<gnc:UIWidget> parent) (<gnc:GncEmployee*> start_selection)
     (<gnc:Book*> book))
   "Dialog: Find a GncEmployee.  Parent and start_selection may be NULL.")

  ;;
  ;; dialog-invoice.h
  ;;

  (gw:wrap-function
   ws
   'gnc:invoice-new
   '<gnc:GncInvoice*>
   "gnc_invoice_new"
   '((<gnc:UIWidget> parent) (<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: create a new GncInvoice.  Parent may be NULL.")

  (gw:wrap-function
   ws
   'gnc:invoice-edit
   '<gw:void>
   "gnc_ui_invoice_window_create"
   '((<gnc:GncInvoice*> invoice))
   "Dialog: Edit a GncInvoice.")

  (gw:wrap-function
   ws
   'gnc:invoice-find
   '<gw:void>
   "gnc_invoice_find"
   '((<gnc:GncInvoice*> start_selection) (<gnc:GncOwner*> owner)
     (<gnc:Book*> book))
   "Dialog: Select a GncInvoice.  Either start_selection or "
   "owner may be NULL.")
  
  ;;
  ;; dialog-job.h
  ;;

  (gw:wrap-function
   ws
   'gnc:job-new
   '<gnc:GncJob*>
   "gnc_job_new"
   '((<gnc:UIWidget> parent) (<gnc:GncOwner*> default_owner)
     (<gnc:Book*> book))
   "Dialog: create a new GncJob.  Parent and Owner may be NULL.")

  (gw:wrap-function
   ws
   'gnc:job-edit
   '<gw:void>
   "gnc_job_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncJob*> job))
   "Dialog: Edit a GncJob.  Parent may be NULL.")

  ;;
  ;; dialog-job-select.h
  ;;

  (gw:wrap-function
   ws
   'gnc:job-find
   '<gw:void>
   "gnc_job_find"
   '((<gnc:UIWidget> parent) (<gnc:GncJob*> job)
     (<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: Find a new job.  Parent and Owner may be NULL.")

  ;;
  ;; dialog-order.h
  ;;

  (gw:wrap-function
   ws
   'gnc:order-new
   '<gnc:GncOrder*>
   "gnc_order_new"
   '((<gnc:UIWidget> parent) (<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: create a new GncOrder.  Parent may be NULL.")

  (gw:wrap-function
   ws
   'gnc:order-edit
   '<gw:void>
   "gnc_ui_order_window_create"
   '((<gnc:GncOrder*> order))
   "Dialog: Edit a GncOrder.")


  (gw:wrap-function
   ws
   'gnc:order-find
   '<gw:void>
   "gnc_order_find"
   '((<gnc:GncOrder*> start_selection) (<gnc:GncOwner*> order_owner)
     (<gnc:Book*> book) )
   "Dialog: Select a GncOrder.  Either start_selection or "
   "order_owner may be NULL.")
  
  ;;
  ;; dialog-vendor.h
  ;;

  (gw:wrap-function
   ws
   'gnc:vendor-new
   '<gnc:GncVendor*>
   "gnc_vendor_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncVendor.  Parent may be NULL.")

  (gw:wrap-function
   ws
   'gnc:vendor-edit
   '<gw:void>
   "gnc_vendor_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncVendor*> vendor))
   "Dialog: Edit a GncVendor.  Parent may be NULL.")


  (gw:wrap-function
   ws
   'gnc:vendor-find
   '<gw:void>
   "gnc_vendor_find"
   '((<gnc:UIWidget> parent) (<gnc:GncVendor*> start_selection)
     (<gnc:Book*> book))
   "Dialog: Select a GncVendor.  Parent and start_selection may be NULL.")
  
)
