;;; -*-scheme-*-

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
   'gnc:customer-select
   '<gnc:GncCustomer*>
   "gnc_customer_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncCustomer*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncCustomer.  Parent and start_selection may be NULL.")

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
   'gnc:employee-select
   '<gnc:GncEmployee*>
   "gnc_employee_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncEmployee*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncEmployee.  Parent and start_selection may be NULL.")

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
   "gnc_invoice_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncInvoice*> invoice))
   "Dialog: Edit a GncInvoice.  Parent may be NULL.")


  (gw:wrap-function
   ws
   'gnc:invoice-select
   '<gnc:GncInvoice*>
   "gnc_invoice_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncInvoice*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncInvoice.  Parent and start_selection may be NULL.")
  
  ;;
  ;; dialog-job.h
  ;;

  (gw:wrap-function
   ws
   'gnc:job-new
   '<gnc:GncJob*>
   "gnc_job_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book)
     (<gnc:GncOwner*> default_owner))
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
   'gnc:job-select
   '<gnc:GncJob*>
   "gnc_ui_select_job_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book)
     (<gnc:GncOwner*> owner) (<gnc:GncJob*> job))
   "Dialog: Select a new job.  Parent and Owner may be NULL.")

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
   "gnc_order_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncOrder*> order))
   "Dialog: Edit a GncOrder.  Parent may be NULL.")


  (gw:wrap-function
   ws
   'gnc:order-select
   '<gnc:GncOrder*>
   "gnc_order_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncOrder*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncOrder.  Parent and start_selection may be NULL.")
  
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
   'gnc:vendor-select
   '<gnc:GncVendor*>
   "gnc_vendor_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncVendor*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncVendor.  Parent and start_selection may be NULL.")
  
)
