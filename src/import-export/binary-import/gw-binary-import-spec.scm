(define-module (g-wrapped gw-app-utils-spec))

(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(let ((mod (gw:new-module "gw-binary-import")))
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

  (gw:module-set-guile-module! mod '(g-wrapped gw-binary-import))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?) 
     (list 
      "#include <druid-commodity.h>\n"
      )))

  (gw:wrap-function
   mod
   'gnc:import-legacy-commodities
   '<gw:void>
   "gnc_import_legacy_commodities"
   '(((<gw:m-chars-caller-owned> gw:const) filename))
   "Launch the legacy-commodity import druid")
)
