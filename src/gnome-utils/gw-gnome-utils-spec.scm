(define-module (g-wrapped gw-gnome-utils-spec))

(use-modules (g-wrap))
(use-modules (g-wrapped gw-engine-spec))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(let ((mod (gw:new-module "gw-gnome-utils")))
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

  (gw:module-set-guile-module! mod '(g-wrapped gw-gnome-utils))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?) 
     (list 
      "#include <gnc-mdi-utils.h>\n"
      )))

  (let ((nnt (gw:wrap-non-native-type
              mod
              '<gnc:mdi-info*>
              "GNCMDIInfo*" "const GNCMDIInfo*")))
    #t)

  (gw:wrap-function
   mod
   'gnc:mdi-has-apps?
   '<gw:bool>
   "gnc_mdi_has_apps"
   '()
   "Return true if there are gnc mdi app windows open.")

  (gw:wrap-function
   mod
   'gnc:mdi-get-current
   '<gnc:mdi-info*>
   "gnc_mdi_get_current"
   '()
   "Return the main window data structure for the application.")

  (gw:wrap-function
   mod
   'gnc:mdi-save 
   '<gw:void>
   "gnc_mdi_save" '((<gnc:mdi-info*> mi) 
                    ((<gw:m-chars-caller-owned>) bookname))
   "Save the MDI window configuration for the specified book")

  (gw:wrap-function
   mod
   'gnc:mdi-restore
   '<gw:void>
   "gnc_mdi_restore" '((<gnc:mdi-info*> mi) 
                       ((<gw:m-chars-caller-owned> gw:const) bookname))
   "Restore MDI window configuration for the specified book")
)
