(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack 2000000)

(let ((mod (gw:new-module "baz-gwrap")))
  (gw:module-depends-on mod "gw-runtime")
  (gw:module-set-declarations-ccodegen! 
   mod 
   (lambda (unused) 
     (list "#include \"baz.h\"\n")))

  (gw:wrap-function
   mod 'baz:hello
   '<gw:bool> "baz_hello"
   '()
   "Print a simple message from C"))
