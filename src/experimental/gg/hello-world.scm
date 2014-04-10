#! /bin/sh
exec guile-gtk -s $0 $*
!#
;; Time-stamp: <1998-03-15 21:45:18 szi>
;;
;; Hello World for Guile-Gtk
;;
;; This is a simple example program, that creates a window with a
;; button labeled "Say Hello". The program prints "Hello World!" when
;; you press the button and exits.
;;

(use-modules (gtk gtk))

(let ((window (gtk-window-new 'toplevel))
      (button (gtk-button-new-with-label "Say Hello")))
  (gtk-window-set-title window "Guile-Gtk: Hello World")
  (gtk-container-border-width window 10)
  (gtk-container-add window button)
  (gtk-signal-connect button "clicked"
		      (lambda () 
			(display "Hello World!")
			(newline)
			(gtk-widget-destroy window)))
  (gtk-widget-show-all window)
  (gtk-standalone-main window))

; Local Variables:
; mode: scheme
; End:
