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
      "#include <dialog-order.h>\n"
      "#include <dialog-payment.h>\n"
      "#include <dialog-tax-table.h>\n"
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
   '<gw:void>
   "gnc_ui_customer_new"
   '((<gnc:Book*> book))
   "Dialog: create a new GncCustomer.")

  (gw:wrap-function
   ws
   'gnc:customer-edit
   '<gw:void>
   "gnc_ui_customer_edit"
   '((<gnc:GncCustomer*> customer))
   "Dialog: Edit a GncCustomer.")

  (gw:wrap-function
   ws
   'gnc:customer-search
   '<gw:void>
   "gnc_customer_search"
   '((<gnc:GncCustomer*> start_selection) (<gnc:Book*> book) )
   "Dialog: Find a GncCustomer.  Start_selection may be NULL.")

  ;;
  ;; dialog-employee.h
  ;;

  (gw:wrap-function
   ws
   'gnc:employee-new
   '<gw:void>
   "gnc_ui_employee_new"
   '((<gnc:Book*> book))
   "Dialog: create a new GncEmployee.")

  (gw:wrap-function
   ws
   'gnc:employee-edit
   '<gw:void>
   "gnc_ui_employee_edit"
   '((<gnc:GncEmployee*> employee))
   "Dialog: Edit a GncEmployee.")

  (gw:wrap-function
   ws
   'gnc:employee-search
   '<gw:void>
   "gnc_employee_search"
   '((<gnc:GncEmployee*> start_selection) (<gnc:Book*> book))
   "Dialog: Find a GncEmployee.  Start_selection may be NULL.")

  ;;
  ;; dialog-invoice.h
  ;;

  (gw:wrap-function
   ws
   'gnc:invoice-new
   '<gw:void>
   "gnc_ui_invoice_new"
   '((<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: create a new GncInvoice.")

  (gw:wrap-function
   ws
   'gnc:invoice-edit
   '<gw:void>
   "gnc_ui_invoice_edit"
   '((<gnc:GncInvoice*> invoice))
   "Dialog: Edit a GncInvoice.")

  (gw:wrap-function
   ws
   'gnc:invoice-search
   '<gw:void>
   "gnc_invoice_search"
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
   '<gw:void>
   "gnc_ui_job_new"
   '((<gnc:GncOwner*> default_owner) (<gnc:Book*> book))
   "Dialog: create a new GncJob.  Owner may be NULL.")

  (gw:wrap-function
   ws
   'gnc:job-edit
   '<gw:void>
   "gnc_ui_job_edit"
   '((<gnc:GncJob*> job))
   "Dialog: Edit a GncJob.")

  (gw:wrap-function
   ws
   'gnc:job-search
   '<gw:void>
   "gnc_job_search"
   '((<gnc:GncJob*> job) (<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: Search for a job.  Job and Owner may be NULL.")

  ;;
  ;; dialog-order.h
  ;;

  (gw:wrap-function
   ws
   'gnc:order-new
   '<gw:void>
   "gnc_ui_order_new"
   '((<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: create a new GncOrder.")

  (gw:wrap-function
   ws
   'gnc:order-edit
   '<gw:void>
   "gnc_ui_order_edit"
   '((<gnc:GncOrder*> order))
   "Dialog: Edit a GncOrder.")


  (gw:wrap-function
   ws
   'gnc:order-search
   '<gw:void>
   "gnc_order_search"
   '((<gnc:GncOrder*> start_selection) (<gnc:GncOwner*> order_owner)
     (<gnc:Book*> book) )
   "Dialog: Select a GncOrder.  Either start_selection or "
   "order_owner may be NULL.")
  
  ;;
  ;; dialog-payment.h
  ;;

  (gw:wrap-function
   ws
   'gnc:payment-new
   '<gw:void>
   "gnc_ui_payment_new"
   '((<gnc:GncOwner*> owner) (<gnc:Book*> book))
   "Dialog: Enter a payment.  The owner may be NULL.")

  ;;
  ;; dialog-tax-table.h
  ;;

  (gw:wrap-function
   ws
   'gnc:tax-table-new
   '<gw:void>
   "gnc_ui_tax_table_window_new"
   '((<gnc:Book*> book))
   "Dialog: Edit the Tax Tables.")

  ;;
  ;; dialog-vendor.h
  ;;

  (gw:wrap-function
   ws
   'gnc:vendor-new
   '<gw:void>
   "gnc_ui_vendor_new"
   '((<gnc:Book*> book))
   "Dialog: create a new GncVendor.")

  (gw:wrap-function
   ws
   'gnc:vendor-edit
   '<gw:void>
   "gnc_ui_vendor_edit"
   '((<gnc:GncVendor*> vendor))
   "Dialog: Edit a GncVendor.")

  (gw:wrap-function
   ws
   'gnc:vendor-search
   '<gw:void>
   "gnc_vendor_search"
   '((<gnc:GncVendor*> start_selection) (<gnc:Book*> book))
   "Dialog: Select a GncVendor.  Start_selection may be NULL.")
  
)
