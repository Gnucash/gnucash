;;; -*-scheme-*-

(debug-set! maxdepth 100000)
(debug-set! stack    2000000)

(define-module (g-wrapped gw-core-utils-spec))

(use-modules (g-wrap))

(display "**** NOTE: this wrapset appears to be empty !?\n")

(let ((ws (gw:new-wrapset "gw-core-utils")))

  (gw:wrapset-set-guile-module! ws '(g-wrapped gw-core-utils))

  (gw:wrapset-add-cs-declarations!
   ws
   (lambda (wrapset client-wrapset)
     (if client-wrapset
         '()
         "#include <core-utils.h>\n"))))
