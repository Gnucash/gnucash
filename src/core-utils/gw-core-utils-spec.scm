;;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-core-utils-spec)
  :use-module (g-wrap))

(let ((mod (gw:new-module "gw-core-utils")))
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
;;  (gw:module-depends-on mod "gw-glib")

  (gw:module-set-guile-module! mod '(g-wrapped gw-core-utils))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (if client-only?
         ""
         (list
          "#include <core-utils.h>\n" ))))

;;  (let ((nnt (gw:wrap-non-native-type
;;              mod
;;              '<gnc:UIWidget>
;;              "gncUIWidget" "const gncUIWidget")))
;;    #t)
  
;;  (gw:wrap-function
;;   mod
;;   'gnc:ui-hierarchy-druid
;;   '<gw:void>
;;   "gnc_ui_hierarchy_druid"
;;   '()
;;   "Open the hiearchy druid for importing an account hierarchy.")

  )
