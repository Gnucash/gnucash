(use-modules (g-wrap))

(debug-set! maxdepth 100000)
(debug-set! stack 2000000)

(use-modules (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "foo-gwrap")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-add-cs-declarations! 
   ws
   (lambda (wrapset client-wrapset) 
     "#include \"foo.h\"\n"))

  (gw:wrap-function
   ws
   'foo:hello
   '<gw:bool> "foo_hello" '()
   "Print a simple message from C"))
