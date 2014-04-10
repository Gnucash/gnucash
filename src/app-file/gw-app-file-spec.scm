(define-module (g-wrapped gw-app-file-spec))
(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(use-modules (g-wrap))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-glib-spec))

(use-modules (g-wrapped gw-engine-spec))

(let ((ws (gw:new-wrapset "gw-app-file")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-depends-on ws "gw-engine")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-app-file))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <gnc-file.h>\n"
      "#include <gnc-file-history.h>\n"
      "#include <gnc-file-dialog.h>\n")))
  
  (gw:wrap-function
   ws
   'gnc:file-query-save
   '<gw:bool>
   "gnc_file_query_save"
   '()
   "Query the user whether to save the current file, and save
if they say 'Yes'. The return is false if the user says 'Cancel'.")

  (gw:wrap-function
   ws
   'gnc:file-quit
   '<gw:void>
   "gnc_file_quit"
   '()
   "Stop working with the current file.")

  (gw:wrap-function
   ws
   'gnc:file-open-file
   '<gw:bool>
   "gnc_file_open_file"
   '(((<gw:mchars> caller-owned const) filename))
   "Open filename.")

  (gw:wrap-function
   ws
   'gnc:history-get-last
   '(<gw:mchars> callee-owned const)
   "gnc_history_get_last"
   '()
   "Get the last file opened by the user.")

  (gw:wrap-function
   ws
   'gnc:file-selection-dialog
   '(<gw:mchars> callee-owned const)
   "gnc_file_dialog"
   '(((<gw:mchars> caller-owned const) title)
     ((<gw:mchars> caller-owned const) filter)
     ((<gw:mchars> caller-owned const) default))
   "Lets the user select a file. Dialog has given title, filter,
or default name. Either filter, default, or both should be NULL.")

  )
