(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack 2000000)

(let ((mod (gw:new-module "foo-gwrap")))
  (gw:module-depends-on mod "gw-runtime")
  (gw:module-set-declarations-ccodegen! 
   mod 
   (lambda (unused) 
     (list "#include \"foo.h\"\n")))

  (gw:wrap-function
   mod 'foo:hello
   '<gw:bool> "foo_hello"
   '()
   "Print a simple message from C"))
