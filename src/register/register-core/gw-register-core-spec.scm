(define-module (g-wrapped gw-register-core-spec))

(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(let ((mod (gw:new-module "gw-register-core")))
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

  (gw:module-set-guile-module! mod '(g-wrapped gw-register-core))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?) 
     (list 
      "#include <basiccell.h>\n"
      )))

  (gw:wrap-non-native-type 
   mod '<gnc:basic-cell>
   "BasicCell*" "const BasicCell*")

  (gw:wrap-function
   mod
   'gnc:basic-cell-get-value
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_basic_cell_get_value"
   '((<gnc:basic-cell> cell))
   "Return the value of the cell.")
  )
