(use-modules (g-wrap))
(debug-set! maxdepth 100000)
(debug-set! stack 2000000)

(use-modules (g-wrap gw-standard-spec))

(let ((ws (gw:new-wrapset "bar-gwrap")))

  (gw:wrapset-depends-on ws "gw-standard")

  (gw:wrapset-add-cs-declarations! 
   ws
   (lambda (wrapset client-wrapset) 
     "#include \"bar.h\"\n"))

  (gw:wrap-function
   ws
   'bar:hello
   '<gw:bool> "bar_hello" '()
   "Print a simple message from C"))
