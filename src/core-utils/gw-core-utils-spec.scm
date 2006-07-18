;;; -*-scheme-*-

(debug-set! maxdepth 100000)
(debug-set! stack    200000)

(define-module (g-wrapped gw-core-utils-spec))

(use-modules (g-wrap))
(use-modules (g-wrap simple-type))

(use-modules (g-wrap gw-standard-spec))
(use-modules (g-wrap gw-wct-spec))
(use-modules (g-wrap gw-glib-spec))

(let ((ws (gw:new-wrapset "gw-core-utils")))

  (gw:wrapset-depends-on ws "gw-standard")
  (gw:wrapset-depends-on ws "gw-wct")
  (gw:wrapset-depends-on ws "gw-glib")

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-core-utils))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (list
      "#include <gnc-gconf-utils.h>\n"
      "#include <gnc-glib-utils.h>\n"
      "#include <gnc-main.h>\n")))

  (gw:wrap-function
   ws
   'gnc:gconf-get-bool
   '<gw:bool>
   "gnc_gconf_get_bool_no_error"
   '(((<gw:mchars> caller-owned) section)
     ((<gw:mchars> caller-owned) name))
   "Get a boolean value from gconf.")

  (gw:wrap-function
   ws
   'gnc:debugging?
   '<gw:bool>
   "gnc_is_debugging"
   '()
   "Is debugging mode on?")

  (gw:wrap-function
   ws
   'g:find-program-in-path
   '(<gw:mchars> callee-owned const)
   "g_find_program_in_path"
   '(((<gw:mchars> caller-owned) program))
   "Get a boolean value from gconf.")

  (gw:wrap-function
   ws
   'gnc:utf8-strip-invalid
   '<gw:void>
   "gnc_utf8_strip_invalid"
   '(((<gw:mchars> caller-owned) program))
   "Strip string of non-utf8 characters.")

)
