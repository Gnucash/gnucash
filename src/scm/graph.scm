
(define (gnc:debug . items)
  (if #t
      (begin
        (display "gnucash: [D] ")
        (for-each (lambda (i) (display i)) items)
        (newline))))

(define gnc:*pi* 3.14159265359)

(define (gnc:_pie-chart-slice_ color width height
                               center-x center-y
                               total
                               start-angle
                               label
                               value)

  (let* ((pie-radius (/ width 3))
          (full-circle (* 2 gnc:*pi*))
          (arc-sweep (* (/ value total) full-circle)))

     (gnc:debug
      color width height center-x center-y total start-angle label value)

     (savestate)
     (filltype 1)
     (colorname color)
     (move 0 0)

     (savestate)
     (frotate (/ (* start-angle 180) gnc:*pi*))
     (cont (inexact->exact pie-radius) 0) 
     (farc 0 0 pie-radius 0
           (* pie-radius (cos arc-sweep))
           (* pie-radius (sin arc-sweep)))
     (cont 0 0)
     (endpath)
  
     (restorestate)
  
     (fmove (/ (* 0.90 width) (* 2 (cos (+ start-angle (/ arc-sweep 2)))))
            (/ (* 0.90 width) (* 2 (sin (+ start-angle (/ arc-sweep 2)))))) 
  
     (alabel (char->integer #\c) (char->integer #\c) label)
    
     (restorestate)
    
     (gnc:debug "start: " start-angle " sweep: " arc-sweep "\n")

     arc-sweep))

(define gnc:_next-wedge-color_
  (let ((colors '("DarkGreen"
                  "FireBrick"
                  "DarkBlue"
                  "DarkOliveGreen"
                  "DarkOrange"
                  "MediumSeaGreen"
                  "peru"
                  "DarkOrchid"
                  "LimeGreen")))
    (set-cdr! (last-pair colors) colors)
    
    (lambda ()
      (set! colors (cdr colors))
      (car colors))))

(define (pie-plotutils chart-items)
  ;; ((label value) (label value) (label value))

  (let ((total 0)
        (current-angle 0)
        (current-color 0))

    ;; get total
    (for-each (lambda (item)
                (gnc:debug (cadr item))
                (set! total (+ total (cadr item))))
              chart-items)

    (for-each
     (lambda (item)
       (let ((width 400)
             (height 400)
             (center-x 200)
             (center-y 200))
         
         (set! current-angle
               (+ current-angle
                  (gnc:_pie-chart-slice_
                   (gnc:_next-wedge-color_)
                   width height center-x center-y
                   total current-angle (car item) (cadr item))))))
     chart-items)))

; static void
; pie_window_expose(GtkDrawingArea *da, GdkEventExpose *event, gpointer data) {
;   //fprintf(stderr, "%p\n", data);
;   //pie_chart(GTK_WIDGET(da), data);

;   {
;     int handle;
;     GdkWindowPrivate *priv = (GdkWindowPrivate *)(GTK_WIDGET(da)->window);

;     assert(parampl("XDRAWABLE_DISPLAY", priv->xdisplay) == 0);
;     assert(parampl("XDRAWABLE_DRAWABLE1", &(priv->xwindow)) == 0);
;     //assert(parampl("XDRAWABLE_DRAWABLE2", &(priv->xwindow)) == 0);
;     assert(parampl("XDRAWABLE_DRAWABLE2", NULL) == 0);
;     handle = newpl("Xdrawable", stdin, stdout, stderr);
;     assert(handle);
;     selectpl(handle);
;     assert(openpl() == 0);
;     space(-200, -200, 200, 200);
;     colorname("grey83");
;     box(-200, -200, 200, 200);
;     pie_plotutils(pie_data);
;     assert(closepl() == 0);
;     selectpl(0);
;     deletepl(handle);
;   }
; }

(define (pie-window)
  (let ((handle #f)
        (result 0))    
    ;; create a Postscript Plotter that writes to standard output
    (set! handle (newpl "X"
                        (get_fileptr_stdin)
                        (get_fileptr_stdout)
                        (get_fileptr_stderr)))
    (if (< handle 0) 
        (begin
          (display "Couldn't create Plotter\n")
          (set! result 1)))
    
    (if (= result 0)
        (begin
          (selectpl handle)           ; select the Plotter for use
          
          (if (< (openpl) 0)          ; open Plotter
              (begin
                (display "Couldn't open Plotter\n")
                (set! result 1)))))

    (space -200 -200 200 200)
    (colorname "grey83")
    (box -200 -200 200 200)
    
    (pie-plotutils '(("horses" 121.32)
                     ("throbbing expanse" 350.19)
                     ("tangibles" 23.32)
                     ("intangibles" 45.44)
                     ("giant fungi" 241.87)))

    (if (< (closepl) 0)          ; close Plotter
        (display "Couldn't close Plotter\n")
        (set! result 1))
    (selectpl 0)                   ; select default Plotter
    (if (< (deletepl handle) 0)    ; delete Plotter we used
        (display "Couldn't delete Plotter\n")
        (set! result 1))))
