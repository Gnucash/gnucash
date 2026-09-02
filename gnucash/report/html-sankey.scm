;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright Brad Hajek <brad-hajek@users.noreply.github.com> 2026
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
(use-modules (ice-9 hash-table))
(use-modules (ice-9 i18n))
(use-modules (srfi srfi-1))
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

(define from-date-string (G_ "From Date"))
(define to-date-string (G_ "To Date"))

(define flow-message (G_ "No cash flow data found."))
(define flow-message-detail (G_ "Ensure you have selected correct dates and accounts with transactions in Options."))

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

(define (make-empty-node name)
  (make-noderecord name 0 0 0 '() '() 0 0 0 0 0 0))

(define (index-nodes-by-name nodes)
  (let ((table (make-hash-table)))
    (for-each (lambda (n)
                (hash-set! table (car n) (cdr n)))
              nodes)
    table))

;; This function processes the list of links to build a unique set of nodes,
;; while also calculating the total incoming and outgoing values for each node.
;; It also associates the relevant links with each node for later use in rendering.
(define (populate-nodes links)
  (let ((nodes '())
        (nodes-by-name (make-hash-table)))
    (define (ensure-node! name)
      (let ((node (hash-ref nodes-by-name name #f)))
        (if node
          node
          (let ((new-node (make-empty-node name)))
            ; (display (format #f "Adding source node: ~a\n" name)) ; troubleshooting output
            (set! nodes (cons (cons name new-node) nodes))
            (hash-set! nodes-by-name name new-node)
            new-node))))
    (for-each (lambda (link)
                (let* ((src (car (car link)))
                       (src-type (cadr (car link)))
                       (dest (car (cadr link)))
                       (dest-type (cadr (cadr link)))
                       (val (caddr link)))
                  ; (display (format #f "Processing link: ~a -> ~a = $~a\n" src dest val)) ; troubleshooting output
                  (let* ((srcnode (ensure-node! src))
                         (destnode (ensure-node! dest))
                         (link-ref (make-linkrecord src src-type dest dest-type val)))
                    (set-noderecord-out-val! srcnode (+ (noderecord-out-val srcnode) val))
                    (set-noderecord-type! srcnode src-type)
                    (set-noderecord-out-links! srcnode (cons link-ref (noderecord-out-links srcnode)))
                    ; (display (format #f "Updated source node: ~a (out-val: ~a)\n" src (noderecord-out-val srcnode))) ; troubleshooting output
                    (set-noderecord-in-val! destnode (+ (noderecord-in-val destnode) val))
                    (set-noderecord-type! destnode dest-type)
                    (set-noderecord-in-links! destnode (cons link-ref (noderecord-in-links destnode)))
                    ; (display (format #f "Updated destination node: ~a (in-val: ~a)\n" dest (noderecord-in-val destnode))) ; troubleshooting output
                    ; (display (format #f "Source node: ~a\n" srcnode)) ; troubleshooting output
                    ; (display (format #f "Destination node: ~a\n" destnode)) ; troubleshooting output
                  )))
      links)
    ;; Reverse per-node link buffers once after accumulation to preserve first-seen link order
    (for-each (lambda (n)
                (let ((node (cdr n)))
                  (set-noderecord-in-links! node (reverse (noderecord-in-links node)))
                  (set-noderecord-out-links! node (reverse (noderecord-out-links node)))))
              nodes)
    ;; Restore first-seen order after O(1) front insertions.
    (reverse nodes)))

(define (noderecord-val n)
  (max (noderecord-in-val n) (noderecord-out-val n)))

;; levels is a mutable hash table so the cache is shared across all recursive calls.
;; visiting is the set of nodes currently on the call stack, used to detect cycles.
(define (get-level name nodes-by-name levels visiting)
  (let ((lvl (hash-ref levels name #f)))
    ; (display (format #f "Processing levels for node: ~a\n" name)) ; troubleshooting output
    (if (number? lvl)
      lvl
      (if (member name visiting)
        0  ; cycle detected — treat node as a root
        (let* ((node (hash-ref nodes-by-name name #f))
               (parents (noderecord-in-links node))
               (computed (if (= (length parents) 0)
                           0
                           (+ 1 (max-parent-level parents nodes-by-name levels (cons name visiting))))))
          (hash-set! levels name computed)
          computed)))))

(define (max-parent-level parents nodes-by-name levels visiting)
  (let ((max-parent-lvl 0))
    (for-each (lambda (parent)
                (let* ((pname (linkrecord-source parent))
                       (plvl (get-level pname nodes-by-name levels visiting)))
                  ; (display (format #f "Processing parent: ~a, parent level: ~a\n" pname plvl)) ; troubleshooting output
                  (if (and (number? plvl) (> plvl max-parent-lvl))
                    (set! max-parent-lvl plvl))))
              parents)
    max-parent-lvl))

(define (populate-levels nodes style)
  (let ((levels (make-hash-table))
        (nodes-by-name (index-nodes-by-name nodes)))
    (for-each (lambda (n)
                (let* ((name (car n))
                       (type (noderecord-type (cdr n))))
                  (if (string=? style "dynamic")
                    ;; dynamic leveling based on longest path from root nodes (those with no in-links)
                    (get-level name nodes-by-name levels '())
                    ;; fixed leveling based on GnuCash account types
                    (hash-set! levels name
                      (case type
                        ((10) 0) ; equity
                        ((8) 1)  ; income
                        ((2) 2)  ; asset
                        ((4) 3)  ; liability
                        ((9) 4)  ; expense
                        (else 5)))))) ; other/unknown types
      nodes)
    levels))

(define (max-level nodes levels)
  (let ((max-lvl 0))
    (for-each (lambda (n)
                (let ((lvl (hash-ref levels (car n) 0)))
                  (if (> lvl max-lvl)
                    (set! max-lvl lvl))))
              nodes)
    max-lvl))

(define (populate-cols max-level nodes levels)
  (let ((cols (make-vector (+ max-level 1) '())))
    (for-each (lambda (n)
                (let* ((name (car n))
                       (level (hash-ref levels name 0)))
                  ; (display (format #f "Populating columns with node: ~a, level: ~a\n" name level)) ; troubleshooting output
                  (vector-set! cols level
                    (cons n (vector-ref cols level)))))
              nodes)
    (filter-map (lambda (col)
                  (if (null? col)
                    #f
                    (reverse col)))
                (vector->list cols))))

;; Calculate the sum of node values in a single column
(define (calculate-col-value col)
  (let ((sum 0))
    (for-each (lambda (node-pair)
                (set! sum (+ sum (noderecord-val (cdr node-pair)))))
              col)
    sum))

;; Sort nodes in a column descending by rendered node value.
(define (sort-col-by-node-val col)
  (sort col
        (lambda (a b)
          (> (noderecord-val (cdr a))
             (noderecord-val (cdr b))))))

;; Build sorted columns and compute global layout stats in one pass.
;; Returns (prepared-cols max-col-val max-col-nodes), where each prepared
;; column is (sorted-col total-col-val col-size).
(define (prepare-cols-for-layout cols)
  (let ((max-col-val 0)
        (max-col-nodes 0)
        (prepared-cols '()))
    (for-each (lambda (col)
                (let* ((sorted-col (sort-col-by-node-val col))
                       (col-size (length sorted-col))
                       (col-val (calculate-col-value sorted-col))
                       (prepared-col (list sorted-col col-val col-size)))
                  (if (> col-val max-col-val)
                    (set! max-col-val col-val))
                  (if (> col-size max-col-nodes)
                    (set! max-col-nodes col-size))
                  (set! prepared-cols (cons prepared-col prepared-cols))))
              cols)
    (list (reverse prepared-cols)
          max-col-val
          max-col-nodes)))

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
(define (populate-node-layout! prepared-cols width height node-width node-padding scale)
  (let* ((num-cols (length prepared-cols))
         (col-width (if (> num-cols 1)
                      (/ (- width node-width) (- num-cols 1))
                      width)))
    (let loop ((col-index 0)
               (remaining prepared-cols)
               (positioned-cols '()))
      (if (null? remaining)
        (reverse positioned-cols)
        (apply (lambda (col total-col-val col-size)
                 (let* ((total-col-height (+ (* total-col-val scale)
                                             (* (- col-size 1) node-padding)))
                        (start-y (/ (- height total-col-height) 2)))
                   (layout-col! col col-index col-width node-width node-padding scale start-y)
                   (loop (+ col-index 1)
                         (cdr remaining)
                         (cons col positioned-cols))))
               (car remaining))))))

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

;; Escape arbitrary strings for safe use in SVG attributes/text nodes.
(define (xml-escape str)
  (let ((s (if (string? str) str (format #f "~a" str))))
    (apply string-append
      (map (lambda (ch)
             (cond
               ((char=? ch #\&) "&amp;")
               ((char=? ch #\<) "&lt;")
               ((char=? ch #\>) "&gt;")
               ((char=? ch #\") "&quot;")
               ((char=? ch #\') "&#39;")
               (else (string ch))))
           (string->list s)))))

(define (account-type->color type income-color expense-color asset-color liability-color equity-color fallback-color)
  (cond
    ((= type 10) equity-color)
    ((= type 8) income-color)
    ((= type 2) asset-color)
    ((= type 4) liability-color)
    ((= type 9) expense-color)
    (else fallback-color)))

;; Convert routed links into SVG <path> elements with nested <title> tooltips.
(define (routed-links->svg-paths routed-links income-color expense-color asset-color liability-color equity-color fallback-color)
  (apply string-append
    (map (lambda (l)
      (apply (lambda (src src-type dst dst-type val x0 x1 y-start y-end dx link-h)
          (let* ((color (account-type->color src-type income-color expense-color asset-color liability-color equity-color fallback-color))
            (stroke-width (if (> link-h 1) link-h 1))
            (path-data (format #f "M~,4f,~,4f C~,4f,~,4f ~,4f,~,4f ~,4f,~,4f"
                x0 y-start (+ x0 dx) y-start (- x1 dx) y-end x1 y-end))
            (title-text (format #f "~a -> ~a: $~,2f" src dst val)))
       (format #f "<path d=\"~a\" fill=\"none\" stroke=\"~a\" stroke-width=\"~,4f\" stroke-opacity=\"0.35\"><title>~a</title></path>"
          (xml-escape path-data)
          (xml-escape color)
          stroke-width
          (xml-escape title-text))))
        l))
         routed-links)))

(define (last-char-index str ch)
  (let loop ((i (- (string-length str) 1)))
    (if (< i 0)
      #f
      (if (char=? (string-ref str i) ch)
        i
        (loop (- i 1))))))

(define (short-account-name name)
  (let ((idx (last-char-index name #\:)))
    (if idx
      (substring name (+ idx 1) (string-length name))
      name)))

;; Convert nodes into SVG <g>, <rect>, <title>, and <text> elements.
(define (nodes->svg-elements nodes width node-width income-color expense-color asset-color liability-color equity-color fallback-color)
  (apply string-append
    (map (lambda (n)
           (let* ((name (car n))
                  (node (cdr n))
                  (color (account-type->color
                           (noderecord-type node)
                           income-color expense-color asset-color liability-color equity-color fallback-color))
                  (rect-h (if (> (noderecord-h node) 2) (noderecord-h node) 2))
                  (is-left-half (< (noderecord-x0 node) (/ width 2)))
                  (text-x (if is-left-half
                            (+ (noderecord-x1 node) 8)
                            (- (noderecord-x0 node) 8)))
                  (text-anchor (if is-left-half "start" "end"))
                  (text-y (+ (noderecord-y0 node) (/ (noderecord-h node) 2) 4))
                  (title-text (format #f "~a: $~,2f" name (noderecord-val node)))
                  (label-text (format #f "~a ($~,0f)" (short-account-name name) (noderecord-val node))))
             (format #f "<g><rect x=\"~,4f\" y=\"~,4f\" width=\"~,4f\" height=\"~,4f\" fill=\"~a\" stroke=\"#2c3e50\" stroke-width=\"1\" rx=\"2\" ry=\"2\"><title>~a</title></rect><text x=\"~,4f\" y=\"~,4f\" text-anchor=\"~a\" font-size=\"8\" fill=\"#2c3e50\" font-weight=\"bold\">~a</text></g>"
                     (noderecord-x0 node)
                     (noderecord-y0 node)
                     node-width
                     rect-h
                     (xml-escape color)
                     (xml-escape title-text)
                     text-x
                     text-y
                     text-anchor
                     (xml-escape label-text))))
         nodes)))

;; Build a complete SVG element string with links and nodes.
(define (svg-node-elements width height routed-links nodes node-width income-color expense-color asset-color liability-color equity-color fallback-color)
  (let ((svg-link-paths (routed-links->svg-paths
                          routed-links
                          income-color
                          expense-color
                          asset-color
                          liability-color
                          equity-color
                          fallback-color))
        (svg-node-markup (nodes->svg-elements
                           nodes
                           width
                           node-width
                           income-color
                           expense-color
                           asset-color
                           liability-color
                           equity-color
                           fallback-color)))
    (format #f "<svg viewBox=\"0 0 ~a ~a\" style=\"width: 100%; height: auto; font-family: sans-serif;\">~a~a</svg>"
            width
            height
            svg-link-paths
            svg-node-markup)))

(define (reset-node-offsets! nodes)
  (for-each (lambda (n)
              (let ((node (cdr n)))
                (set-noderecord-source-offset! node 0)
                (set-noderecord-target-offset! node 0)))
            nodes))

;; Decorate raw links with precomputed routing keys and node references to
;; avoid repeated hash lookups inside sort comparisons and routing loop.
;; Output tuple format:
;; (src-y dst-y src-name src-type dst-name dst-type val snode tnode)
(define (decorate-link-for-routing link nodes-by-name)
  (let* ((src-name (raw-link-source-name link))
      (src-type (raw-link-source-type link))
      (dst-name (raw-link-target-name link))
      (dst-type (raw-link-target-type link))
      (val (raw-link-value link))
      (snode (hash-ref nodes-by-name src-name #f))
      (tnode (hash-ref nodes-by-name dst-name #f)))
    (list (noderecord-y0 snode)
    (noderecord-y0 tnode)
    src-name
    src-type
    dst-name
    dst-type
    val
    snode
    tnode)))

;; Route links in Scheme; SVG elements are rendered in Scheme.
;; Output tuple format:
;; (source s-type target t-type value x0 x1 y-start y-end dx link-h)
(define (route-links! links nodes scale)
  (let* ((nodes-by-name (index-nodes-by-name nodes))
         (decorated-links (map (lambda (l) (decorate-link-for-routing l nodes-by-name)) links))
         (sorted-links (sort decorated-links
                             (lambda (a b)
                               (let ((a-src-y (car a))
                                     (b-src-y (car b)))
                                 (if (< a-src-y b-src-y)
                                   #t
                                   (if (> a-src-y b-src-y)
                                     #f
                                     (< (cadr a) (cadr b)))))))))
    (reset-node-offsets! nodes)
    (let loop ((remaining sorted-links)
               (acc '()))
      (if (null? remaining)
        (reverse acc)
        (apply (lambda (src-y dst-y src-name src-type dst-name dst-type val snode tnode)
                 (let* ((link-h (* val scale))
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
                               acc))))
               (car remaining))))))

(define (gnc:html-sankey-render sankey doc)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (links (gnc:html-sankey-links sankey))
         (width (gnc:html-sankey-width sankey))
         (height (gnc:html-sankey-height sankey))
         (node-padding 18)
         (node-width 24))

    (push (format #f "<p>~a: <b>~a</b></p>\n" from-date-string (gnc:html-sankey-from-date sankey)))
    (push (format #f "<p>~a: <b>~a</b></p>\n" to-date-string (gnc:html-sankey-to-date sankey)))
    (push (format #f "<div id=sankey_chart style='width: 100%; height: auto; background: #fafafa; border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; box-sizing: border-box;'>"))

    (if (null? links)
      ;; skip SVG output and just show a message if no data
      (begin
        (push (format #f "  <h4>~a</h4>\n" flow-message))
        (push (format #f "  <p>~a</p>\n" flow-message-detail))
        (push "</div>\n"))
      ;; otherwise render the chart
      (let* ((nodes (populate-nodes links))
             (style (gnc:html-sankey-x-axis-style sankey))
             (levels (populate-levels nodes style))
             (max-lvl (max-level nodes levels))
             (cols (populate-cols max-lvl nodes levels))
             (layout-prep (prepare-cols-for-layout cols))
             (prepared-cols (car layout-prep))
             (max-col-val (cadr layout-prep))
             (max-col-nodes (caddr layout-prep))
             (usable-height (- height (* (+ max-col-nodes 1) node-padding)))
             (scale (if (> max-col-val 0)
              (/ usable-height max-col-val)
              1))
             (routed-links (begin
                             (populate-node-layout! prepared-cols width height node-width node-padding scale)
                             (route-links! links nodes scale)))
             (svg-markup (svg-node-elements
                          width
                          height
                          routed-links
                          nodes
                          node-width
                          (gnc:html-sankey-income-color sankey)
                          (gnc:html-sankey-expense-color sankey)
                          (gnc:html-sankey-asset-color sankey)
                          (gnc:html-sankey-liability-color sankey)
                          (gnc:html-sankey-equity-color sankey)
                          (gnc:html-sankey-fallback-color sankey))))
        (begin
          (push svg-markup)
          (push "</div>\n"))))
  retval))
