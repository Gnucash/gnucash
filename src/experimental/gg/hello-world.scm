;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
