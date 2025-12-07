(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (tests test-engine-extras))
(use-modules (tests test-report-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-html-chart.scm")
  (test-html-chart)
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
