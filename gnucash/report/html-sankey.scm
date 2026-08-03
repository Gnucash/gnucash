;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; By Brad Hajek <brad-hajek@users.noreply.github.com>
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-module (gnucash report html-sankey))

(use-modules (gnucash core-utils))
(use-modules (gnucash report html-utilities))
(use-modules (gnucash report report-utilities))
(use-modules (ice-9 format))
(use-modules (srfi srfi-9))

(export gnc:html-sankey?)
(export gnc:make-html-sankey)

(export gnc:html-sankey-set-from-date!)
(export gnc:html-sankey-set-to-date!)

(export gnc:html-sankey-set-width!)
(export gnc:html-sankey-set-height!)

(export gnc:html-sankey-set-x-axis-style!)

(export gnc:html-sankey-set-income-color!)
(export gnc:html-sankey-set-expense-color!)
(export gnc:html-sankey-set-asset-color!)
(export gnc:html-sankey-set-liability-color!)
(export gnc:html-sankey-set-equity-color!)
(export gnc:html-sankey-set-fallback-color!)

(export gnc:html-sankey-set-links!)

(export gnc:html-sankey-render)

(define-record-type <html-sankey>
  (make-html-sankey from-date to-date width height x-axis-style income-color expense-color asset-color liability-color equity-color fallback-color links)
  html-sankey?
  (from-date html-sankey-from-date html-sankey-set-from-date!)
  (to-date html-sankey-to-date html-sankey-set-to-date!)
  (width html-sankey-width html-sankey-set-width!)
  (height html-sankey-height html-sankey-set-height!)
  (x-axis-style html-sankey-x-axis-style html-sankey-set-x-axis-style!)
  (income-color html-sankey-income-color html-sankey-set-income-color!)
  (expense-color html-sankey-expense-color html-sankey-set-expense-color!)
  (asset-color html-sankey-asset-color html-sankey-set-asset-color!)
  (liability-color html-sankey-liability-color html-sankey-set-liability-color!)
  (equity-color html-sankey-equity-color html-sankey-set-equity-color!)
  (fallback-color html-sankey-fallback-color html-sankey-set-fallback-color!)
  (links html-sankey-links html-sankey-set-links!))

(define gnc:html-sankey? html-sankey?)

(define gnc:html-sankey-from-date html-sankey-from-date)
(define gnc:html-sankey-set-from-date! html-sankey-set-from-date!)
(define gnc:html-sankey-to-date html-sankey-to-date)
(define gnc:html-sankey-set-to-date! html-sankey-set-to-date!)

(define gnc:html-sankey-width html-sankey-width)
(define gnc:html-sankey-set-width! html-sankey-set-width!)
(define gnc:html-sankey-height html-sankey-height)
(define gnc:html-sankey-set-height! html-sankey-set-height!)

(define gnc:html-sankey-x-axis-style html-sankey-x-axis-style)
(define gnc:html-sankey-set-x-axis-style! html-sankey-set-x-axis-style!)

(define gnc:html-sankey-income-color html-sankey-income-color)
(define gnc:html-sankey-set-income-color! html-sankey-set-income-color!)
(define gnc:html-sankey-expense-color html-sankey-expense-color)
(define gnc:html-sankey-set-expense-color! html-sankey-set-expense-color!)
(define gnc:html-sankey-asset-color html-sankey-asset-color)
(define gnc:html-sankey-set-asset-color! html-sankey-set-asset-color!)
(define gnc:html-sankey-liability-color html-sankey-liability-color)
(define gnc:html-sankey-set-liability-color! html-sankey-set-liability-color!)
(define gnc:html-sankey-equity-color html-sankey-equity-color)
(define gnc:html-sankey-set-equity-color! html-sankey-set-equity-color!)
(define gnc:html-sankey-fallback-color html-sankey-fallback-color)
(define gnc:html-sankey-set-fallback-color! html-sankey-set-fallback-color!)

(define gnc:html-sankey-links html-sankey-links)
(define gnc:html-sankey-set-links! html-sankey-set-links!)

(define (gnc:make-html-sankey)
  (make-html-sankey
  '()                              ;from-date
  '()                              ;to-date
  '()                              ;width
  '()                              ;height
  '()                              ;x-axis-style
  '()                              ;income-color'
  '()                              ;expense-color'
  '()                              ;asset-color'
  '()                              ;liability-color'
  '()                              ;equity-color'
  '()                              ;fallback-color'
  '()                              ;links
  ))

(define-record-type <linkrecord>
  (make-linkrecord source s-type target t-type value)
  linkrecord?
  (source linkrecord-source)
  (s-type linkrecord-s-type)
  (target linkrecord-target)
  (t-type linkrecord-t-type)
  (value linkrecord-value))

(define-record-type <noderecord>
  (make-noderecord name type in-val out-val in-links out-links
             x0 x1 y0 h source-offset target-offset)
  noderecord?
  (name          noderecord-name)
  (type          noderecord-type    set-noderecord-type!)
  (in-val        noderecord-in-val  set-noderecord-in-val!)
  (out-val       noderecord-out-val set-noderecord-out-val!)
  (in-links      noderecord-in-links  set-noderecord-in-links!)
  (out-links     noderecord-out-links set-noderecord-out-links!)
  (x0            noderecord-x0  set-noderecord-x0!)
  (x1            noderecord-x1  set-noderecord-x1!)
  (y0            noderecord-y0  set-noderecord-y0!)
  (h             noderecord-h   set-noderecord-h!)
  (source-offset noderecord-source-offset set-noderecord-source-offset!)
  (target-offset noderecord-target-offset set-noderecord-target-offset!))

(define (make-link src src-type dest dest-type val)
  (make-linkrecord src src-type dest dest-type val))

(define (make-empty-node name)
  (make-noderecord name 0 0 0 '() '() 0 0 0 0 0 0))

(define (adjoin-node name nodes)
  (if (assoc name nodes)
    (begin
      ; (display (format #f "Source node already exists: ~a\n" name)) ; troubleshooting output
      nodes)
    (begin
      ; (display (format #f "Adding source node: ~a\n" name)) ; troubleshooting output
      (append nodes (list (cons name (make-empty-node name)))))))

;; This function processes the list of links to build a unique set of nodes,
;; while also calculating the total incoming and outgoing values for each node.
;; It also associates the relevant links with each node for later use in rendering.
(define (populate-nodes links)
  (let ((nodes '()))
    (for-each (lambda (link)
                (let* ((src (car (car link)))
                       (src-type (cadr (car link)))
                       (dest (car (cadr link)))
                       (dest-type (cadr (cadr link)))
                       (val (caddr link)))
                  ; (display (format #f "Processing link: ~a -> ~a = $~a\n" src dest val)) ; troubleshooting output
                  (set! nodes (adjoin-node src nodes))
                  (set! nodes (adjoin-node dest nodes))
                  (let* ((srcnode (assoc-ref nodes src))
                         (destnode (assoc-ref nodes dest))
                         (link-ref (make-linkrecord src src-type dest dest-type val)))
                    (set-noderecord-out-val! srcnode (+ (noderecord-out-val srcnode) val))
                    (set-noderecord-type! srcnode src-type)
                    (set-noderecord-out-links! srcnode (append (list link-ref) (noderecord-out-links srcnode)))
                    ; (display (format #f "Updated source node: ~a (out-val: ~a)\n" src (noderecord-out-val srcnode))) ; troubleshooting output
                    (set-noderecord-in-val! destnode (+ (noderecord-in-val destnode) val))
                    (set-noderecord-type! destnode dest-type)
                    (set-noderecord-in-links! destnode (append (list link-ref) (noderecord-in-links destnode)))
                    ; (display (format #f "Updated destination node: ~a (in-val: ~a)\n" dest (noderecord-in-val destnode))) ; troubleshooting output
                    ; (display (format #f "Source node: ~a\n" srcnode)) ; troubleshooting output
                    ; (display (format #f "Destination node: ~a\n" destnode)) ; troubleshooting output
                  )))
      links)
    nodes))

(define (noderecord-val n)
  (max (noderecord-in-val n) (noderecord-out-val n)))

(define (adjoin-levels name level levels)
  (if (assoc name levels)
    levels
    (append levels (list (cons name level)))))

;; levels-cell is a mutable (list <alist>) so the cache is shared across all recursive calls.
;; visiting is the set of nodes currently on the call stack, used to detect cycles.
(define (get-level name nodes levels-cell visiting)
  (let ((lvl (assoc-ref (car levels-cell) name)))
    ; (display (format #f "Processing levels for node: ~a\n" name)) ; troubleshooting output
    (if (number? lvl)
      lvl
      (if (member name visiting)
        0  ; cycle detected — treat node as a root
        (let* ((node (assoc-ref nodes name))
               (parents (noderecord-in-links node))
               (computed (if (= (length parents) 0)
                           0
                           (+ 1 (max-parent-level parents nodes levels-cell (cons name visiting))))))
          (set-car! levels-cell (adjoin-levels name computed (car levels-cell)))
          computed)))))

(define (max-parent-level parents nodes levels-cell visiting)
  (let ((max-parent-lvl 0))
    (for-each (lambda (parent)
                (let* ((pname (linkrecord-source parent))
                       (plvl (get-level pname nodes levels-cell visiting)))
                  ; (display (format #f "Processing parent: ~a, parent level: ~a\n" pname plvl)) ; troubleshooting output
                  (if (and (number? plvl) (> plvl max-parent-lvl))
                    (set! max-parent-lvl plvl))))
              parents)
    max-parent-lvl))

(define (populate-levels nodes style)
  (let ((levels-cell (list '())))
    (for-each (lambda (n)
                (let* ((name (car n))
                       (type (noderecord-type (cdr n))))
                  (if (string=? style "dynamic")
                    ;; dynamic leveling based on longest path from root nodes (those with no in-links)
                    (get-level name nodes levels-cell '())
                    ;; fixed leveling based on GnuCash account types
                    (set-car! levels-cell
                      (adjoin-levels name
                        (case type
                          ((10) 0) ; equity
                          ((8) 1)  ; income
                          ((2) 2)  ; asset
                          ((4) 3)  ; liability
                          ((9) 4)  ; expense
                          (else 5)) ; other/unknown types
                        (car levels-cell))))))
      nodes)
    (car levels-cell)))

(define (max-level levels)
  (let ((max-lvl 0))
    (for-each (lambda (lvl)
                (if (> (cdr lvl) max-lvl)
                  (set! max-lvl (cdr lvl))))
              levels)
    max-lvl))

(define (populate-cols max-level nodes levels)
  (let ((cols (make-vector (+ max-level 1) '())))
    (for-each (lambda (n)
                (let* ((name (car n))
                       (level (assoc-ref levels name)))
                  ; (display (format #f "Populating columns with node: ~a, level: ~a\n" name level)) ; troubleshooting output
                  (vector-set! cols level
                    (append (vector-ref cols level) (list n)))))
              nodes)
    (filter (lambda (col) (not (null? col)))
            (vector->list cols))))

;; Calculate the sum of node values in a single column
(define (calculate-col-value col)
  (let ((sum 0))
    (for-each (lambda (node-pair)
                (set! sum (+ sum (noderecord-val (cdr node-pair)))))
              col)
    sum))

;; Calculate maximum column value and maximum nodes count from cols
;; Returns a pair (max-col-val . max-col-nodes)
(define (calculate-col-stats cols)
  (let ((max-col-val 0)
        (max-col-nodes 0))
    (for-each (lambda (col)
                (let ((col-val (calculate-col-value col)))
                  (if (> col-val max-col-val)
                    (set! max-col-val col-val))
                  (if (> (length col) max-col-nodes)
                    (set! max-col-nodes (length col)))))
              cols)
    (cons max-col-val max-col-nodes)))

;; Minimal list sorter to avoid depending on modules not present in older Guile builds.
(define (list-sort pred lst)
  (define (insert x sorted)
    (if (null? sorted)
      (list x)
      (if (pred x (car sorted))
        (cons x sorted)
        (cons (car sorted) (insert x (cdr sorted))))))
  (let loop ((rest lst)
             (acc '()))
    (if (null? rest)
      acc
      (loop (cdr rest) (insert (car rest) acc)))))

;; Sort nodes in a column descending by rendered node value.
(define (sort-col-by-node-val col)
  (list-sort
    (lambda (a b)
      (> (noderecord-val (cdr a))
         (noderecord-val (cdr b))))
    col))

;; Mutate nodes in one column with x/y/height and reset link offsets.
(define (layout-col! col col-index col-width node-width node-padding scale start-y)
  (let loop ((remaining col)
             (current-y start-y))
    (if (null? remaining)
      #t
      (let* ((node (cdar remaining))
             (x0 (* col-index col-width))
             (h (* (noderecord-val node) scale)))
        (set-noderecord-x0! node x0)
        (set-noderecord-x1! node (+ x0 node-width))
        (set-noderecord-y0! node current-y)
        (set-noderecord-h! node h)
        (set-noderecord-source-offset! node 0)
        (set-noderecord-target-offset! node 0)
        (loop (cdr remaining)
              (+ current-y h node-padding))))))

;; Compute node geometry by column and return sorted/positioned columns.
(define (populate-node-layout! cols width height node-width node-padding scale)
  (let* ((num-cols (length cols))
         (col-width (if (> num-cols 1)
                      (/ (- width node-width) (- num-cols 1))
                      width)))
    (let loop ((col-index 0)
               (remaining cols)
               (positioned-cols '()))
      (if (null? remaining)
        (reverse positioned-cols)
        (let* ((col (sort-col-by-node-val (car remaining)))
               (total-col-val (calculate-col-value col))
               (total-col-height (+ (* total-col-val scale)
                                    (* (- (length col) 1) node-padding)))
               (start-y (/ (- height total-col-height) 2)))
          (layout-col! col col-index col-width node-width node-padding scale start-y)
          (loop (+ col-index 1)
                (cdr remaining)
                (cons col positioned-cols)))))))

;; Raw link helpers for readability.
(define (raw-link-source-name link)
  (car (car link)))

(define (raw-link-source-type link)
  (cadr (car link)))

(define (raw-link-target-name link)
  (car (cadr link)))

(define (raw-link-target-type link)
  (cadr (cadr link)))

(define (raw-link-value link)
  (caddr link))

(define (reset-node-offsets! nodes)
  (for-each (lambda (n)
              (let ((node (cdr n)))
                (set-noderecord-source-offset! node 0)
                (set-noderecord-target-offset! node 0)))
            nodes))

(define (link-routing< a b nodes)
  (let* ((a-src (assoc-ref nodes (raw-link-source-name a)))
         (b-src (assoc-ref nodes (raw-link-source-name b)))
         (a-src-y (noderecord-y0 a-src))
         (b-src-y (noderecord-y0 b-src)))
    (if (< a-src-y b-src-y)
      #t
      (if (> a-src-y b-src-y)
        #f
        (let* ((a-dst (assoc-ref nodes (raw-link-target-name a)))
               (b-dst (assoc-ref nodes (raw-link-target-name b)))
               (a-dst-y (noderecord-y0 a-dst))
               (b-dst-y (noderecord-y0 b-dst)))
          (< a-dst-y b-dst-y))))))

;; Route links in Scheme so JS only needs to emit SVG path and text elements.
;; Output tuple format:
;; (source s-type target t-type value x0 x1 y-start y-end dx link-h)
(define (route-links! links nodes scale)
  (let ((sorted-links (list-sort (lambda (a b) (link-routing< a b nodes)) links)))
    (reset-node-offsets! nodes)
    (let loop ((remaining sorted-links)
               (acc '()))
      (if (null? remaining)
        (reverse acc)
        (let* ((l (car remaining))
               (src-name (raw-link-source-name l))
               (src-type (raw-link-source-type l))
               (dst-name (raw-link-target-name l))
               (dst-type (raw-link-target-type l))
               (val (raw-link-value l))
               (snode (assoc-ref nodes src-name))
               (tnode (assoc-ref nodes dst-name))
               (link-h (* val scale))
               (y-start (+ (noderecord-y0 snode)
                           (noderecord-source-offset snode)
                           (/ link-h 2)))
               (y-end (+ (noderecord-y0 tnode)
                         (noderecord-target-offset tnode)
                         (/ link-h 2)))
               (x0 (noderecord-x1 snode))
               (x1 (noderecord-x0 tnode))
               (dx (/ (- x1 x0) 2)))
          (set-noderecord-source-offset! snode (+ (noderecord-source-offset snode) link-h))
          (set-noderecord-target-offset! tnode (+ (noderecord-target-offset tnode) link-h))
          (loop (cdr remaining)
                (cons (list src-name src-type dst-name dst-type val x0 x1 y-start y-end dx link-h)
                      acc)))))))

;;
;; Scheme->JS functions are temporary until we migrate everything to Scheme only rendering
;;

;; Escape arbitrary strings so they are safe inside single-quoted JavaScript strings.
(define (js-escape str)
  (let ((s (if (string? str) str (format #f "~a" str))))
    (apply string-append
      (map (lambda (ch)
            (cond
              ((char=? ch #\\) "\\\\")
              ((char=? ch #\') "\\'")
              ((char=? ch #\newline) "\\n")
              ((char=? ch #\return) "\\r")
              ((char=? ch #\tab) "\\t")
              ((char=? ch #\<) "\\u003C")
              ((char=? ch #\>) "\\u003E")
              ((char=? ch #\&) "\\u0026")
              (else (string ch))))
        (string->list s)))))

;; This function converts the links data structure into a JavaScript array of objects string.
(define (links->js-array links)
  (string-append "["
    (string-join
      (map (lambda (link)
            (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f}"
              (js-escape (car (car link))) ;; Source account name
              (cadr (car link))          ;; Source account type
              (js-escape (car (cadr link))) ;; Destination account name
              (cadr (cadr link))         ;; Destination account type
              (caddr link)))             ;; Flow value
          links)
      ",")
    "]"))

;; Convert pre-routed links generated by route-links! into JS objects.
(define (routed-links->js-array routed-links)
  (string-append "["
    (string-join
      (map (lambda (l)
            (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f, x0: ~,4f, x1: ~,4f, yStart: ~,4f, yEnd: ~,4f, dx: ~,4f, linkH: ~,4f}"
              (js-escape (list-ref l 0))
              (list-ref l 1)
              (js-escape (list-ref l 2))
              (list-ref l 3)
              (list-ref l 4)
              (list-ref l 5)
              (list-ref l 6)
              (list-ref l 7)
              (list-ref l 8)
              (list-ref l 9)
              (list-ref l 10)))
          routed-links)
      ",")
    "]"))

;; This function converts the nodes data structure into a JavaScript object literal string.
(define (nodes->js-array nodes)
  (string-append "{"
    (string-join
      (map (lambda (n)
            (let ((name (car n))
                  (node (cdr n)))
              (format #f "'~a': {'name': '~a', 'type': ~d, 'val': ~,2f, 'inLinks': [~a], 'outLinks': [~a], 'x0': ~,4f, 'x1': ~,4f, 'y0': ~,4f, 'h': ~,4f, 'sourceOffset': ~,4f, 'targetOffset': ~,4f}"
                (js-escape name)          ;; Account name
                (js-escape (noderecord-name node)) ;; Account name
                (noderecord-type node) ;; Account type
                (noderecord-val node)  ;; Max of in/out value for sizing
                (string-join
                  (map (lambda (l)
                        (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f}"
                          (js-escape (linkrecord-source l))  ;; Source account name
                          (linkrecord-s-type l)  ;; Source account type
                          (js-escape (linkrecord-target l))  ;; Destination account name
                          (linkrecord-t-type l)  ;; Destination account type
                          (linkrecord-value l))) ;; Flow value
                    (noderecord-in-links node))  ;; In-links list of records
                  ",")
                (string-join
                  (map (lambda (l)
                        (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f}"
                          (js-escape (linkrecord-source l))  ;; Source account name
                          (linkrecord-s-type l)  ;; Source account type
                          (js-escape (linkrecord-target l))  ;; Destination account name
                          (linkrecord-t-type l)  ;; Destination account type
                          (linkrecord-value l))) ;; Flow value
                    (noderecord-out-links node)) ;; Out-links list of records
                  ",")
                (noderecord-x0 node)      ;; left x
                (noderecord-x1 node)      ;; right x
                (noderecord-y0 node)      ;; top y
                (noderecord-h node)       ;; height
                (noderecord-source-offset node) ;; source offset
                (noderecord-target-offset node) ;; target offset
                )))
          nodes)
      ",")
    "}"))

;; This function converts the levels data structure into a JavaScript object literal string.
(define (levels->js-array levels)
  (string-append "{"
    (string-join
      (map (lambda (lvl)
            (format #f "'~a': ~d" (js-escape (car lvl)) (cdr lvl)))
          levels)
      ",")
    "}"))

;; This function converts the cols data structure into a JavaScript array of arrays string.
;; Each element emits nodes['name'] so that col entries are live references to the nodes dict,
;; ensuring mutations (x0, y0, h, sourceOffset, targetOffset) are reflected in both structures.
(define (cols->js-array cols)
  (string-append "["
    (string-join
      (map (lambda (col)
            (string-append "["
              (string-join
                (map (lambda (n)
                      (format #f "nodes['~a']" (js-escape (car n))))
                    col)
                ",")
              "]"))
          cols)
      ",")
    "]"))

;;
;; Inline CSS and JavaScript for rendering the Sankey chart
;;

;; This function generates the inline CSS style string for the chart container div, based on the specified height.
(define (chart-div-style height)
  (format #f "style='width: 100%;
  height: ~apx;
  background: #fafafa;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  padding: 20px;
  box-sizing: border-box;'
  " (* 2.5 height)))

;; This is the inline CSS style string for the message div that displays errors or no-data messages.
(define message-div-style "style='padding: 10px;
  font-family: sans-serif;'
  ")

;; Dynamic color picker based on GnuCash account type
(define js-node-color "
  function getNodeColor(type) {
    //console.log('Account type: ' + type);
    if (type == 10) return equityColor;
    if (type == 8) return incomeColor;
    if (type == 2) return assetColor;
    if (type == 4) return liabilityColor;
    if (type == 9) return expenseColor;
    return fallbackColor;
  }")

(define js-sankey "
  var chartDiv = document.getElementById('sankey_chart');
  var messageDiv = document.getElementById('sankey_message');

  function escapeHtml(str) {
    return String(str)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/\"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  try {
    // 5. GENERATE SVG
    var svgParts = [];
    svgParts.push('<svg viewBox=\"0 0 ' + width + ' ' + height + '\" style=\"width: 100%; height: auto; font-family: sans-serif;\">');

    // DRAW LINKS (Bezier S-Curves)
    for (var i = 0; i < links.length; i++) {
      var l = links[i];
      var linkH = l.linkH;
      var yStart = l.yStart;
      var yEnd = l.yEnd;
      var x0 = l.x0;
      var x1 = l.x1;
      var dx = l.dx;

      var color = getNodeColor(l.s_type);
      var pathData = 'M' + x0 + ',' + yStart + ' C' + (x0 + dx) + ',' + yStart + ' ' + (x1 - dx) + ',' + yEnd + ' ' + x1 + ',' + yEnd;

      svgParts.push('<path d=\"' + pathData + '\" fill=\"none\" stroke=\"' + color + '\" stroke-width=\"' + Math.max(1, linkH) + '\" stroke-opacity=\"0.35\">');
      svgParts.push('  <title>' + escapeHtml(l.source) + ' &rarr; ' + escapeHtml(l.target) + ': $' + l.value.toFixed(2) + '</title>');
      svgParts.push('</path>');
    }

    // DRAW NODES & LABELS
    for (var name in nodes) {
      var node = nodes[name];
      var color = getNodeColor(node.type);
      var shortName = name.split(':').pop();

      svgParts.push('<g>');
      svgParts.push('  <rect x=\"' + node.x0 + '\" y=\"' + node.y0 + '\" width=\"' + nodeWidth + '\" height=\"' + Math.max(2, node.h) + '\" fill=\"' + color + '\" stroke=\"#2c3e50\" stroke-width=\"1\" rx=\"2\" ry=\"2\">');
      svgParts.push('    <title>' + escapeHtml(name) + ': $' + node.val.toFixed(2) + '</title>');
      svgParts.push('  </rect>');

      // Dynamic label placement (Left aligned on left side of chart, right aligned on right side)
      var isLeftHalf = node.x0 < (width / 2);
      var textX = isLeftHalf ? (node.x1 + 8) : (node.x0 - 8);
      var textAnchor = isLeftHalf ? 'start' : 'end';
      var textY = node.y0 + (node.h / 2) + 4;

      svgParts.push('  <text x=\"' + textX + '\" y=\"' + textY + '\" text-anchor=\"' + textAnchor + '\" font-size=\"8\" fill=\"#2c3e50\" font-weight=\"bold\">');
      svgParts.push(     escapeHtml(shortName) + ' ($' + node.val.toFixed(0) + ')');
      svgParts.push('  </text>');
      svgParts.push('</g>');
    }

    svgParts.push('</svg>');
    chartDiv.innerHTML = svgParts.join('\\n');

  } catch (err) {
    messageDiv.innerHTML = '<h4>Sankey Visualizer Error</h4><pre>' + err.message + '</pre><p>Try to reduce the date range or the number of accounts selected in Options.</p>';
    messageDiv.style.color = 'red';
  }")

(define (gnc:html-sankey-render sankey doc)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (links (gnc:html-sankey-links sankey))
         (js-links-raw (links->js-array links)))

    (push (format #f "<p>From Date: <b>~a</b></p>\n" (gnc:html-sankey-from-date sankey)))
    (push (format #f "<p>To Date: <b>~a</b></p>\n" (gnc:html-sankey-to-date sankey)))
    (push (format #f "<div id=sankey_chart ~a>\n" (chart-div-style (gnc:html-sankey-height sankey))))
    (push (format #f "  <div id=sankey_message ~a>\n" message-div-style))

    (if (or (string=? js-links-raw "[]") (null? js-links-raw))
      ;; skip the javascript rendering and just show a message if no data
      (begin
        (push "    <h4>No cash flow data found.</h4>\n")
        (push "    <p>Ensure you have selected correct dates and accounts with transactions in Options.</p>\n")
        (push "  </div>\n")
        (push "</div>\n"))
      ;; otherwise render the chart
      (let* ((nodes (populate-nodes links))
             (style (gnc:html-sankey-x-axis-style sankey))
             (levels (populate-levels nodes style))
             (js-levels (levels->js-array levels))
             (max-lvl (max-level levels))
             (cols (populate-cols max-lvl nodes levels))
             (col-stats (calculate-col-stats cols))
             (max-col-val (car col-stats))
             (max-col-nodes (cdr col-stats))
             (width (gnc:html-sankey-width sankey))
             (height (gnc:html-sankey-height sankey))
             (node-padding 18)
             (node-width 24)
             (usable-height (- height (* (+ max-col-nodes 1) node-padding)))
             (scale (if (> max-col-val 0)
              (/ usable-height max-col-val)
              1))
             (positioned-cols (populate-node-layout! cols width height node-width node-padding scale))
             (routed-links (route-links! links nodes scale))
             (js-links (routed-links->js-array routed-links))
             (js-nodes (nodes->js-array nodes))
             (js-cols (cols->js-array positioned-cols)))
        (begin
          (push "  </div>\n")
          (push "</div>\n")
          ; (push (format #f "<!-- nodes: ~a -->\n" nodes)) ; troubleshooting output
          ; (push (format #f "<!-- links: ~a -->\n" links)) ; troubleshooting output
          (push "<script>\n")
          (push "(function () {\n")
          (push "  // AGGREGATE LINKS DATA\n")
          (push (format #f "  var links = ~a;\n\n" js-links))
          (push "  // DISCOVER UNIQUE NODES & ACCUMULATE TOTALS\n")
          (push (format #f "  var nodes = ~a;\n\n" js-nodes))
          (push "  // TOPOLOGICAL LEVELING (X-AXIS COLUMNS) \n")
          (push (format #f "  var levels = ~a;\n" js-levels))
          (push (format #f "  var maxLevel = ~a;\n" max-lvl))
          (push (format #f "  var cols = ~a;\n" js-cols))
          (push "  var numCols = cols.length;\n\n")
          (push "  // SVG/NODE SIZING CONFIG\n")
          (push (format #f "  var width = ~a;\n" (gnc:html-sankey-width sankey)))
          (push (format #f "  var height = ~a;\n" (gnc:html-sankey-height sankey)))
          (push (format #f "  var nodePadding = ~a;\n" node-padding))
          (push (format #f "  var nodeWidth = ~a;\n\n" node-width))
          (push "  // SCALING (NODE POSITIONS PRECOMPUTED IN SCHEME)\n")
          (push (format #f "  var maxColVal = ~a;\n" max-col-val))
          (push (format #f "  var maxColNodes = ~a;\n\n" max-col-nodes))
          (push "  var usableHeight = height - (maxColNodes + 1) * nodePadding;\n")
          (push "  var scale = maxColVal > 0 ? (usableHeight / maxColVal) : 1;\n")
          (push "  // Kept for link thickness and future layout parity checks\n")
          (push "  var colWidth = numCols > 1 ? ((width - nodeWidth) / (numCols - 1)) : width;\n")
          (push "  // SVG/NODE COLOR CONFIG\n")
          (push (format #f "  var incomeColor = '~a';\n" (gnc:html-sankey-income-color sankey)))
          (push (format #f "  var expenseColor = '~a';\n" (gnc:html-sankey-expense-color sankey)))
          (push (format #f "  var assetColor = '~a';\n" (gnc:html-sankey-asset-color sankey)))
          (push (format #f "  var liabilityColor = '~a';\n" (gnc:html-sankey-liability-color sankey)))
          (push (format #f "  var equityColor = '~a';\n" (gnc:html-sankey-equity-color sankey)))
          (push (format #f "  var fallbackColor = '~a';\n" (gnc:html-sankey-fallback-color sankey)))
          (push (format #f "~a;\n" js-node-color))
          (push (format #f "~a;\n" js-sankey))
          (push "})();")
          (push "</script>\n"))))
  retval))
