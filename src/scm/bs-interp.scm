;;;; startup-interpreter.scm -*-scheme-*-

;; Load the necessary files for use in interpreter mode.

(primitive-load (getenv "GNC_BOOTSTRAP_SCM"))
(gnc:load "startup.scm")
(gnc:load "main.scm")
(gnc:startup)

