;;; -*-scheme-*-
(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-app-file-spec)
  :use-module (g-wrap))

(use-modules (g-wrapped gw-engine-spec))
(use-modules (g-wrapped gw-glib-spec))

(let ((mod (gw:new-module "gw-app-file")))
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
  (gw:module-depends-on mod "gw-glib")

  (gw:module-set-guile-module! mod '(g-wrapped gw-app-file))

  (gw:module-set-declarations-ccodegen!
   mod
   (lambda (client-only?)
     (list
      "#include <gnc-file.h>\n"
      "#include <gnc-file-history.h>\n"
      "#include <gnc-file-dialog.h>\n")))


  (gw:wrap-function
   mod
   'gnc:file-query-save
   '<gw:bool>
   "gnc_file_query_save"
   '()
   "Query the user whether to save the current file, and save
if they say 'Yes'. The return is false if the user says 'Cancel'.")

  (gw:wrap-function
   mod
   'gnc:file-quit
   '<gw:void>
   "gnc_file_quit"
   '()
   "Stop working with the current file.")

  (gw:wrap-function
   mod
   'gnc:file-open-file
   '<gw:bool>
   "gnc_file_open_file"
   '(((<gw:m-chars-caller-owned> gw:const) filename))
   "Open filename.")

  (gw:wrap-function
   mod
   'gnc:history-get-last
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_history_get_last"
   '()
   "Get the last file opened by the user.")

  (gw:wrap-function
   mod
   'gnc:file-selection-dialog
   '(<gw:m-chars-callee-owned> gw:const)
   "gnc_file_dialog"
   '(((<gw:m-chars-caller-owned> gw:const) title)
     ((<gw:m-chars-caller-owned> gw:const) filter)
     ((<gw:m-chars-caller-owned> gw:const) default))
   "Lets the user select a file. Dialog has given title, filter,
or default name. Either filter, default, or both should be NULL.")

  )
