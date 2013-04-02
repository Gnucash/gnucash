#! /bin/sh
exec gnomeg -s $0 $*
!#
(use-modules (gnome gnome)
	     (gtk gtk)
	     (gtk gdk))


;; Use a nicer font IMO, if available */
(let* ((font "-adobe-helvetica-medium-r-normal--*-100-*-*-*-*-*-*")
       (default-style (gtk-widget-get-default-style))
       (new-font (gdk-font-load font)))

  ;(if (and default-style new-font)
  ;    st->font = f;
  ;    }
  ;  }

  (exit 0))

;;(gnome-init-hack "canvas" #f '())

(let* ((win (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new #f 0))
       (zoom-a (gtk-adjustment-new 1 0.2 5.1 0.05 0.05 0.1))
       (zoom-s (gtk-hscale-new zoom-a))
       (canvas (gnome-canvas-new))
       (root (gnome-canvas-root canvas)))

  (define (create-draggable-item type x1 y1 x2 y2 color)
    (let* ((item (gnome-canvas-item-new root type
				    'x1 x1 'y1 y1 'x2 x2 'y2 y2
				    'width_pixels 1
				    'outline_color "black"
				    'fill_color color))
	   (last-x #f)
	   (last-y #f)
	   (dragging #f))
      (define (handler ev)
	(case (gdk-event-type ev)
	  ((enter-notify)
	   (gnome-canvas-item-set item 'width_units 2))
	  ((leave-notify)
	   (gnome-canvas-item-set item 'width_pixels 0))
	  ((button-press)
	   (case (gdk-event-button ev)
	     ((1)
	      (set! last-x (gdk-event-x ev))
	      (set! last-y (gdk-event-y ev))
	      (set! dragging #t))))
	  ((button-release)
	   (set! dragging #f))
	  ((motion-notify)
	   (if dragging
	       (let ((x (gdk-event-x ev))
		     (y (gdk-event-y ev)))
		 (gnome-canvas-item-move item (- x last-x) (- y last-y))
		    (set! last-x x)
		    (set! last-y y))))))
      (gtk-signal-connect item "event" handler)))
      
  (gtk-container-add win vbox)
  (gtk-box-pack-start vbox canvas)
  (gtk-box-pack-start vbox zoom-s #f)
  ;;(gnome-canvas-set-size canvas 300 300)
  (gtk-widget-set-usize canvas 300 300)
  (gtk-widget-show-all win)

  (create-draggable-item 'GnomeCanvasRect 50 50 150 150 "red")
  (create-draggable-item 'GnomeCanvasEllipse 100 100 200 200 "green")

  (gtk-signal-connect zoom-a "value-changed"
		      (lambda () 
			(let ((val (gtk-adjustment-value zoom-a)))
			  (gnome-canvas-set-pixels-per-unit canvas val))))

  (gtk-standalone-main win))
