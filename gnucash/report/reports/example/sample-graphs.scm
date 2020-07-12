;; -*-scheme-*-
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


;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

(define-module (gnucash reports example sample-graphs))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))
(use-modules (gnucash html))
(use-modules (gnucash report))

;; Add this module to enable translatable strings
;; Use (N_ string) to mark string for translation (it won't be translated on the spot)
;; Use (G_ string) to use a translation of this string if it exists.
(use-modules (gnucash core-utils))

;; It's common to define frequently used strings once
;; This also helps getting more consistent messages which simplifies
;; the life of translators later on.
(define reportname (N_ "Sample Graphs"))

(define (simple-pie-chart)
  (let ((chart (gnc:make-html-chart)))
    ;; the minimum chartjs-based html-chart requires the following settings
    (gnc:html-chart-set-type! chart 'pie)

    ;; title is either a string, or a list of strings
    (gnc:html-chart-set-title! chart "Pie Chart Title")
    (gnc:html-chart-set-width! chart '(pixels . 480))
    (gnc:html-chart-set-height! chart '(pixels . 360))

    ;; data-labels and data-series should be the same length
    (gnc:html-chart-set-data-labels! chart '("alpha" "beta" "delta"))
    (gnc:html-chart-add-data-series! chart
                                     "series-1"             ;series name
                                     '(25 45 30)            ;pie ratios
                                     (gnc:assign-colors 3)) ;colours

    ;; piechart doesn't need axes display:
    (gnc:html-chart-set-axes-display! chart #f)
    chart))

(define (simple-bar-chart stacked?)
  (let ((chart (gnc:make-html-chart))
        (colours (gnc:assign-colors 3)))
    (gnc:html-chart-set-title!
     chart (list "Bar Chart Title"
                 (gnc:html-string-sanitize "Bar Chart SubTitle")))
    (gnc:html-chart-set-type! chart 'bar)
    (gnc:html-chart-set-width! chart '(pixels . 480))
    (gnc:html-chart-set-height! chart '(pixels . 360))
    (gnc:html-chart-set-data-labels! chart '("water" "fire"))
    (gnc:html-chart-add-data-series! chart
                                     "dataset1"
                                     '(25 75)
                                     (car colours))
    (gnc:html-chart-add-data-series! chart
                                     "dataset2"
                                     '(45 55)
                                     (cadr colours))
    (gnc:html-chart-add-data-series! chart
                                     "dataset3"
                                     '(30 70)
                                     (caddr colours))

    (gnc:html-chart-set-stacking?! chart stacked?)
    (gnc:html-chart-set-y-axis-label! chart "Y Axis")
    chart))

(define (simple-scatter-chart)
  (let ((chart (gnc:make-html-chart)))
    (gnc:html-chart-set-type! chart 'line)
    (gnc:html-chart-set-title! chart "Scatter Title")

    (gnc:html-chart-set-data-labels! chart '("a" "b" "c" "d" "e"))
    (gnc:html-chart-set-width! chart '(pixels . 480))
    (gnc:html-chart-set-height! chart '(pixels . 360))

    ;; scatter plots require x/y coordinates for data points
    (gnc:html-chart-add-data-series! chart
                                     "xy-plot"
                                     '(((x . 25) (y . 75))
                                       ((x . 45) (y . 55))
                                       ((x . 32) (y . 30)))
                                     (gnc:assign-colors 3)
                                     'borderWidth 1
                                     'fill #f)

    ;; The following sets xaxis to linear rather than category and
    ;; also illustrates how to precisely select chartjs options. The
    ;; second parameter is a list of symbols to target a particular
    ;; chartJS option. See chartJS documentation.
    (gnc:html-chart-set! chart '(options scales xAxes (0) type) "linear")

    chart))

(define (options-generator)
  (gnc:new-options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (test-graphing-renderer report-obj)
  ;; These are some helper functions for looking up option values.
  (define options (gnc:report-options report-obj))
  (define (get-op section name)
    (gnc:lookup-option options section name))
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  (let ((document (gnc:make-html-document)))

    (gnc:html-document-set-title! document (G_ reportname))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p (G_ "Pie:"))))
    (gnc:html-document-add-object! document (simple-pie-chart))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p (G_ "Bar, normal:"))))
    (gnc:html-document-add-object! document (simple-bar-chart #f))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p (G_ "Bar, stacked:"))))
    (gnc:html-document-add-object! document (simple-bar-chart #t))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p (G_ "Scatter:"))))
    (gnc:html-document-add-object! document (simple-scatter-chart))

    document))

;; Here we define the actual report with gnc:define-report
(gnc:define-report

 ;; The version of this report.
 'version 1

 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name reportname

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "2eff1729072e411ab124065b850dee6a"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name (G_ reportname)

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (G_ reportname)

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-example)

 ;; The options generator function defined above.
 'options-generator options-generator

 ;; The rendering function defined above.
 'renderer test-graphing-renderer)
