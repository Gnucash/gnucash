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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define gnc:*pi* 3.14159265359)

(define gnc:pie-chart-colors '("DarkGreen"
                               "FireBrick"
                               "DarkBlue"
                               "DarkOliveGreen"
                               "DarkOrange"
                               "MediumSeaGreen"
                               "peru"
                               "DarkOrchid"
                               "LimeGreen"))

(set-cdr! (last-pair gnc:pie-chart-colors) gnc:pie-chart-colors)


(define (gnc:_pie-chart-slice_ color width height
                               center-x center-y
                               total
                               start-angle
                               label
                               value)
  
  (let* ((pie-radius (/ width 3))
         (full-circle (* 2 gnc:*pi*))
         (arc-sweep (* (/ value total) full-circle)))
    
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
     
     (let ((label-offset (* 0.90 (/ width 3))))
       (fmove (/ label-offset (* 2 (cos (+ start-angle (/ arc-sweep 2)))))
              (/ label-offset (* 2 (sin (+ start-angle (/ arc-sweep 2)))))))
     
     (colorname "black")
     (alabel (char->integer #\c) (char->integer #\c) label)
    
     (restorestate)
    
     arc-sweep))

(define gnc:_next-wedge-color_ #f)
(define gnc:_reset-wedge-colors_ #f)

(let ((current-color gnc:pie-chart-colors))
  (set! gnc:_next-wedge-color_
        (lambda ()
          (set! current-color (cdr current-color))
          (car current-color)))
  
  (set! gnc:_reset-wedge-colors_
        (lambda ()
          (set! current-color gnc:pie-chart-colors))))

(define (pie-plotutils width height chart-items)
  ;; chart-items -> ((label value) (label value) (label value))

  (let ((total 0)
        (current-angle 0)
        (current-color 0))

    ;; get total
    (for-each (lambda (item)
                (set! total (+ total (cadr item))))
              chart-items)

    (for-each
     (lambda (item)
       (let ((center-x 0)
             (center-y 0))
         
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

(define (pie-legend width height names colors)
  
  (savestate)
  (let ((max-name-width (apply max (map flabelwidth names)))
        (half-width (/ width 2))
        (half-height (/ height 2))
        (font-height 12)
        (inner-border 4))
    
    (savestate)
    (colorname "grey83")
    (fbox (- half-width) (- half-height) half-width half-height)
    (restorestate)
    
    (colorname "black")
    (fmove (+ (- half-width) inner-border)
           (- half-height inner-border font-height))
    
    (for-each
     (lambda (label)
       (alabel (char->integer #\l) (char->integer #\x) label)
       (fmoverel (- (flabelwidth label)) (- font-height)))
     names)
    
    (restorestate)))

(define (pie-test-drawing func)
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
    
    (func)

    (if (< (closepl) 0)          ; close Plotter
        (display "Couldn't close Plotter\n")
        (set! result 1))
    (selectpl 0)                   ; select default Plotter
    (if (< (deletepl handle) 0)    ; delete Plotter we used
        (display "Couldn't delete Plotter\n")
        (set! result 1))))

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


(define (pie-combination)
  
  (pie-test-drawing
   (lambda ()
     (let ((items '(("horses" 121.32)
                    ("throbbing expanse" 350.19)
                    ("tangibles" 23.32)
                    ("intangibles" 45.44)
                    ("giant fungi" 241.87))))
       
       (savestate)
       (ftranslate -60 0)
       (pie-plotutils 400 400 items)
       (restorestate)

       (savestate)
       (ftranslate 180 -100)
       (pie-legend 150 100 (map car items) "")
       (restorestate)))))



(define (text-test)
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
    (box -180 -180 180 180)
    
    (savestate)

    (filltype 1)
    (colorname "DarkGreen")
    (move 0 0)
    
    ;;(savestate)
    ;;(frotate (/ (* start-angle 180) gnc:*pi*))
    ;;(cont (inexact->exact pie-radius) 0) 
    ;;(farc 0 0 pie-radius 0
    ;;      (* pie-radius (cos arc-sweep))
    ;;      (* pie-radius (sin arc-sweep)))
    ;;(cont 0 0)
    ;;(endpath)
    
    ;;(restorestate)
    
    ;;(fmove (/ (* 0.90 width) (* 2 (cos (+ start-angle (/ arc-sweep 2)))))
    ;;       (/ (* 0.90 width) (* 2 (sin (+ start-angle (/ arc-sweep 2)))))) 
    
    (alabel (char->integer #\c) (char->integer #\c) "Rampage!")
    
    (restorestate)


    (if (< (closepl) 0)          ; close Plotter
        (display "Couldn't close Plotter\n")
        (set! result 1))
    (selectpl 0)                   ; select default Plotter
    (if (< (deletepl handle) 0)    ; delete Plotter we used
        (display "Couldn't delete Plotter\n")
        (set! result 1))))
