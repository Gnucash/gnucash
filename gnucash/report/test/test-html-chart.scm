(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-13))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (tests test-engine-extras))
(use-modules (tests test-report-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-html-chart.scm")
  (test-html-chart)
  (test-html-chart-api)
  (test-html-chart-render)
  (test-end "test-html-chart.scm"))


(define (test-html-chart)

  (let ((chart (gnc:make-html-chart))
        (a-list-of-pairs '((unit . day)
                           (displayFormats (day . "DD-MM-YYYY"))
                           (tooltipFormat . "DD-MM-YYYY"))))

    (gnc:html-chart-add-data-series! chart "label" '(2 3 4) "red")

    ;; general setters and getters
    (gnc:html-chart-set! chart '(data datasets (0) data) #(1 2 3))
    (test-equal "data setter & getter"
      #(1 2 3)
      (gnc:html-chart-get chart '(data datasets (0) data)))

    (gnc:html-chart-set! chart '(type) 'scatter)
    (test-equal "type setter & getter"
      'scatter
      (gnc:html-chart-get chart '(type)))

    ;; options setters and getters
    ;;
    ;; options are stored in a nested list of pairs, in which the car
    ;; is a symbol, the cdr is either a value (bool/string/number)
    ;; another list of pairs, or another simple list

    ;; (list (cons 'maintainAspectRatio #f)
    ;;       (cons 'chartArea (list (cons 'backgroundColor "white")))
    ;;       (cons 'scales (list (cons 'xAxes (list
    ;;                                         (list (cons 'display #t)
    ;;                                               (cons 'gridlines (list (cons 'display #t)
    ;;                                                                      (cons 'lineWidth 1.5)))
    ;;                                               (cons 'ticks (list (cons 'fontSize 12)))))))))

    ;; traversal is accomplished as a list of symbols or numbers
    ;; e.g. '(maintainAspectRatio), '(chartArea backgroundColor),
    ;; '(scales xAxes (0) display). NOTE: xAxes specifies a number to
    ;; identify the kth element in the list. this is required as per
    ;; chartjs specification.
    ;;
    ;; syntax is: (gnc:html-chart-set! chart path newval) or
    ;; (gnc:html-chart-get chart path)

    (gnc:html-chart-set! chart '(options maintainAspectRatio) 'abc)
    (test-equal "root option setter & getter"
      'abc
      (gnc:html-chart-get chart '(options maintainAspectRatio)))

    (test-error "path doesn't exist"
      #t
      (gnc:html-chart-get chart '(options scales xAxes (0) time)))

    (gnc:html-chart-set! chart '(options scales xAxes (0) time) a-list-of-pairs)

    (test-equal "path exists and the list-of-pairs is intact"
      a-list-of-pairs
      (gnc:html-chart-get chart '(options scales xAxes (0) time)))

    (gnc:html-chart-set! chart '(options legend position) 'de)
    (test-equal "1st level option setter & getter"
      'de
      (gnc:html-chart-get chart '(options legend position)))

    (test-error
     "1st level option fails - cannot traverse through existing path"
     'wrong-type-arg
     (gnc:html-chart-set! chart '(options legend position invalid) 'de))

    (test-error "deep nested new path - nonexistent"
      'invalid-path
      (gnc:html-chart-get chart '(create new nested path)))

    (gnc:html-chart-set! chart '(create new nested path) 'newpath)
    (test-equal "created deep nested new path"
      'newpath
      (gnc:html-chart-get chart '(create new nested path)))

    (gnc:html-chart-set! chart '(create list-kth (4) nested path) 'k4th)
    (test-equal "deep nested new path - created 4th list item"
      'k4th
      (gnc:html-chart-get chart '(create list-kth (4) nested path)))

    (gnc:html-chart-set! chart '(create list-kth (1) nested path) 'k1th)
    (test-equal "deep nested new path - created 1th list item"
      'k1th
      (gnc:html-chart-get chart '(create list-kth (1) nested path)))

    (gnc:html-chart-set! chart '(create list-kth (0) nested path) 'k0th)
    (test-equal "deep nested new path - created 0th list item"
      'k0th
      (gnc:html-chart-get chart '(create list-kth (0) nested path)))

    (test-equal "deep nested new path - 4th list item intact"
      'k4th
      (gnc:html-chart-get chart '(create list-kth (4) nested path)))

    (gnc:html-chart-set! chart '(create list-kth (3)) 'three)
    (test-equal "deep nested new path - 3th list item is the last path"
      'three
      (gnc:html-chart-get chart '(create list-kth (3))))

    (test-error
     "deep nested new path - cannot set 6th index"
     'index-too-high
     (gnc:html-chart-set! chart '(create list-kth (6) nested path) 'k4th))
    ))


;; the setters below are the interface the reports use. they are
;; tested against the option paths of the charting library, so that a
;; library upgrade shows up here as an explicit change.
(define (test-html-chart-api)
  (test-begin "html-chart api")

  (let ((chart (gnc:make-html-chart)))

    ;; size, currency and number format
    (gnc:html-chart-set-width! chart '(pixels . 480))
    (gnc:html-chart-set-height! chart '(percent . 50))
    (test-equal "set-width!" '(pixels . 480) (gnc:html-chart-width chart))
    (test-equal "set-height!" '(percent . 50) (gnc:html-chart-height chart))

    (gnc:html-chart-set-currency-iso! chart "EUR")
    (gnc:html-chart-set-currency-symbol! chart "$")
    (gnc:html-chart-set-format-style! chart "percent")
    (test-equal "set-currency-iso!" "EUR" (gnc:html-chart-currency-iso chart))
    (test-equal "set-currency-symbol!" "$" (gnc:html-chart-currency-symbol chart))
    (test-equal "set-format-style!" "percent" (gnc:html-chart-format-style chart))

    ;; chart type. a pie chart requires the pointer to be over a slice
    (test-equal "default type is bar" 'bar (gnc:html-chart-type chart))
    (gnc:html-chart-set-type! chart 'pie)
    (test-equal "set-type! pie" 'pie (gnc:html-chart-type chart))
    (test-equal "set-type! pie requires intersect"
      #t (gnc:html-chart-get chart '(options tooltips intersect)))
    (gnc:html-chart-set-type! chart 'line)
    (test-equal "set-type! line does not require intersect"
      #f (gnc:html-chart-get chart '(options tooltips intersect)))

    ;; title - a string, or a list of strings for a multi-line title
    (gnc:html-chart-set-title! chart "single line")
    (test-equal "set-title! string" "single line" (gnc:html-chart-title chart))
    (gnc:html-chart-set-title! chart '("first line" "second line"))
    (test-equal "set-title! list becomes a vector"
      #("first line" "second line") (gnc:html-chart-title chart))
    (test-equal "set-title! writes into the chart options"
      #("first line" "second line") (gnc:html-chart-get chart '(options title text)))

    (gnc:html-chart-set-data-labels! chart '("a" "b" "c"))
    (test-equal "set-data-labels! becomes a vector"
      #("a" "b" "c") (gnc:html-chart-get chart '(data labels)))

    ;; axes
    (gnc:html-chart-set-axes-display! chart #f)
    (test-equal "set-axes-display! x-axis"
      #f (gnc:html-chart-get chart '(options scales xAxes (0) display)))
    (test-equal "set-axes-display! y-axis"
      #f (gnc:html-chart-get chart '(options scales yAxes (0) display)))

    (gnc:html-chart-set-x-axis-type! chart 'linear)
    (test-equal "set-x-axis-type!"
      'linear (gnc:html-chart-get chart '(options scales xAxes (0) type)))

    (gnc:html-chart-set-x-axis-label! chart "x-label")
    (test-equal "set-x-axis-label!"
      "x-label" (gnc:html-chart-get chart '(options scales xAxes (0) scaleLabel labelString)))
    (gnc:html-chart-set-y-axis-label! chart "y-label")
    (test-equal "set-y-axis-label!"
      "y-label" (gnc:html-chart-get chart '(options scales yAxes (0) scaleLabel labelString)))

    (gnc:html-chart-set-stacking?! chart #t)
    (test-equal "set-stacking?! x-axis"
      #t (gnc:html-chart-get chart '(options scales xAxes (0) stacked)))
    (test-equal "set-stacking?! y-axis"
      #t (gnc:html-chart-get chart '(options scales yAxes (0) stacked)))

    (gnc:html-chart-set-grid?! chart #f)
    (test-equal "set-grid?! x-axis"
      #f (gnc:html-chart-get chart '(options scales xAxes (0) gridLines display)))
    (test-equal "set-grid?! y-axis"
      #f (gnc:html-chart-get chart '(options scales yAxes (0) gridLines display))))

  ;; data series
  (let ((chart (gnc:make-html-chart)))
    (test-equal "a new chart has no data series"
      #() (gnc:html-chart-get chart '(data datasets)))

    (gnc:html-chart-add-data-series! chart "series-1" '(1 2 3) "red")
    (gnc:html-chart-add-data-series! chart "series-2" '(4 5 6) '("blue" "green")
                                     'urls '("url-1" "url-2")
                                     'fill #f)

    (test-equal "add-data-series! appends"
      2 (vector-length (gnc:html-chart-get chart '(data datasets))))
    (test-equal "series label"
      "series-1" (gnc:html-chart-get chart '(data datasets (0) label)))
    (test-equal "series data becomes a vector"
      #(1 2 3) (gnc:html-chart-get chart '(data datasets (0) data)))
    (test-equal "a single colour sets backgroundColor"
      "red" (gnc:html-chart-get chart '(data datasets (0) backgroundColor)))
    (test-equal "a single colour sets borderColor"
      "red" (gnc:html-chart-get chart '(data datasets (0) borderColor)))
    (test-equal "a list of colours becomes a vector"
      #("blue" "green") (gnc:html-chart-get chart '(data datasets (1) backgroundColor)))
    (test-equal "additional keys are added to the series - urls"
      #("url-1" "url-2") (gnc:html-chart-get chart '(data datasets (1) urls)))
    (test-equal "additional keys are added to the series - fill"
      #f (gnc:html-chart-get chart '(data datasets (1) fill)))

    (gnc:html-chart-clear-data-series! chart)
    (test-equal "clear-data-series!"
      #() (gnc:html-chart-get chart '(data datasets))))

  (test-end "html-chart api"))

(define (render-chart chart)
  (string-concatenate (reverse (gnc:html-chart-render chart #f))))

(define (rendered-canvas-id html)
  (let ((m (string-match "<canvas id=\"([^\"]+)\"" html)))
    (and m (match:substring m 1))))

(define (test-html-chart-render)
  (test-begin "html-chart render")

  (let ((chart (gnc:make-html-chart)))
    (gnc:html-chart-set-currency-iso! chart "USD")
    (gnc:html-chart-set-title! chart "the title")
    (gnc:html-chart-set-data-labels! chart '("a" "b"))
    (gnc:html-chart-add-data-series! chart "series" '(1 2) "red")

    (let ((html (render-chart chart)))
      (test-assert "renders a canvas"
        (string-contains html "<canvas id="))
      (test-assert "creates the chart"
        (string-contains html "new Chart(chartid, chartjsoptions)"))
      (test-assert "passes the currency to the number formatter"
        (string-contains html "var curriso = \"USD\""))
      (test-assert "renders the options as json"
        (string-contains html "var chartjsoptions = {"))
      (test-assert "renders the data series"
        (string-contains html "\"label\" : \"series\""))
      (test-assert "sets the tooltip label callback"
        (string-contains html "chartjsoptions.options.tooltips.callbacks.label = tooltipLabel;"))
      (test-assert "sets the tooltip title callback"
        (string-contains html "chartjsoptions.options.tooltips.callbacks.title = tooltipTitle;"))
      (test-assert "formats the x-axis ticks by default"
        (string-contains html "chartjsoptions.options.scales.xAxes[0].ticks.callback"))
      (test-assert "formats the y-axis ticks by default"
        (string-contains html "chartjsoptions.options.scales.yAxes[0].ticks.callback")))

    ;; a report may want the axis ticks left alone, see price-scatter
    (gnc:html-chart-set-custom-x-axis-ticks?! chart #f)
    (gnc:html-chart-set-custom-y-axis-ticks?! chart #f)
    (let ((html (render-chart chart)))
      (test-assert "custom x-axis ticks can be disabled"
        (not (string-contains html "chartjsoptions.options.scales.xAxes[0].ticks.callback")))
      (test-assert "custom y-axis ticks can be disabled"
        (not (string-contains html "chartjsoptions.options.scales.yAxes[0].ticks.callback")))))

  ;; a multicolumn report renders several charts into the same page
  (let* ((html-1 (render-chart (gnc:make-html-chart)))
         (html-2 (render-chart (gnc:make-html-chart)))
         (id-1 (rendered-canvas-id html-1)))
    (test-assert "the canvas is given an id" id-1)
    (test-assert "each chart is given its own id"
      (not (equal? id-1 (rendered-canvas-id html-2))))
    (test-assert "the jump anchor uses the chart id"
      (string-contains html-1 (string-append "jump-" id-1))))

  (test-end "html-chart render"))
