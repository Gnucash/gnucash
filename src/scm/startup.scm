;;;; startup.scm -*-scheme-*-

;; Load all the files we need from wherever the user has specified.
;; None of these loads will be affected by any command line arguments
;; since arguments aren't parsed until gnc:main is executed.

(gnc:load "macros.scm")
(gnc:load "config-var.scm")
(gnc:load "utilities.scm")
(gnc:load "path.scm")
(gnc:load "c-interface.scm")
(gnc:load "prefs.scm")
(gnc:load "command-line.scm")
(gnc:load "convenience-wrappers.scm")
(gnc:load "hooks.scm")
(gnc:load "main.scm")
