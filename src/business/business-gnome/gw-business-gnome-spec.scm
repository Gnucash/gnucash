;;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-business-gnome-spec)
  :use-module (g-wrap))

(use-modules (g-wrapped gw-business-core-spec))
(use-modules (g-wrapped gw-gnc-spec))

(let ((mod (gw:new-module "gw-business-gnome")))
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
  (gw:module-depends-on mod "gw-business-core")
  (gw:module-depends-on mod "gw-engine")
  (gw:module-depends-on mod "gw-gnc")
  
  (gw:module-set-guile-module! mod '(g-wrapped gw-business-gnome))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <dialog-customer.h>\n"
      "#include <dialog-employee.h>\n"
      "#include <dialog-job.h>\n"
      "#include <dialog-job-select.h>\n"
      "#include <dialog-order.h>\n"
      "#include <dialog-vendor.h>\n"
      )))

  (gw:module-set-init-ccodegen!
   mod
   (lambda (client-only?) 
     (if client-only? 
         '()
         (gw:inline-scheme '(use-modules (gnucash business-gnome))))))

  ;;
  ;; dialog-customer.h
  ;;

  (gw:wrap-function
   mod
   'gnc:customer-new
   '<gnc:GncCustomer*>
   "gnc_customer_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncCustomer.  Parent may be NULL.")

  (gw:wrap-function
   mod
   'gnc:customer-edit
   '<gw:void>
   "gnc_customer_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncCustomer*> customer))
   "Dialog: Edit a GncCustomer.  Parent may be NULL.")


  (gw:wrap-function
   mod
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
   mod
   'gnc:employee-new
   '<gnc:GncEmployee*>
   "gnc_employee_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncEmployee.  Parent may be NULL.")

  (gw:wrap-function
   mod
   'gnc:employee-edit
   '<gw:void>
   "gnc_employee_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncEmployee*> employee))
   "Dialog: Edit a GncEmployee.  Parent may be NULL.")


  (gw:wrap-function
   mod
   'gnc:employee-select
   '<gnc:GncEmployee*>
   "gnc_employee_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncEmployee*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncEmployee.  Parent and start_selection may be NULL.")

  ;;
  ;; dialog-job.h
  ;;

  (gw:wrap-function
   mod
   'gnc:job-new
   '<gnc:GncJob*>
   "gnc_job_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book)
     (<gnc:GncCustomer*> default_customer))
   "Dialog: create a new GncJob.  Parent and Customer may be NULL.")

  (gw:wrap-function
   mod
   'gnc:job-edit
   '<gw:void>
   "gnc_job_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncJob*> job))
   "Dialog: Edit a GncJob.  Parent may be NULL.")

  ;;
  ;; dialog-job-select.h
  ;;

  (gw:wrap-function
   mod
   'gnc:job-select
   '<gnc:GncJob*>
   "gnc_ui_select_job_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book)
     (<gnc:GncCustomer*> cust) (<gnc:GncJob*> job))
   "Dialog: Select a new job.  Parent and Customer may be NULL.")

  ;;
  ;; dialog-order.h
  ;;

  (gw:wrap-function
   mod
   'gnc:order-new
   '<gnc:GncOrder*>
   "gnc_order_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncOrder.  Parent may be NULL.")

  (gw:wrap-function
   mod
   'gnc:order-edit
   '<gw:void>
   "gnc_order_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncOrder*> order))
   "Dialog: Edit a GncOrder.  Parent may be NULL.")


  (gw:wrap-function
   mod
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
   mod
   'gnc:vendor-new
   '<gnc:GncVendor*>
   "gnc_vendor_new"
   '((<gnc:UIWidget> parent) (<gnc:Book*> book))
   "Dialog: create a new GncVendor.  Parent may be NULL.")

  (gw:wrap-function
   mod
   'gnc:vendor-edit
   '<gw:void>
   "gnc_vendor_edit"
   '((<gnc:UIWidget> parent) (<gnc:GncVendor*> vendor))
   "Dialog: Edit a GncVendor.  Parent may be NULL.")


  (gw:wrap-function
   mod
   'gnc:vendor-select
   '<gnc:GncVendor*>
   "gnc_vendor_edit_new_select"
   '((<gnc:Book*> book) (<gnc:GncVendor*> start_selection)
     (<gnc:UIWidget> parent))
   "Dialog: Select a GncVendor.  Parent and start_selection may be NULL.")
  
)
