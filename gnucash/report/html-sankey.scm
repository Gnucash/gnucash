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

;;
;; Scheme->JS functions are temporary until we migrate everything to Scheme only rendering
;;

;; This function converts the links data structure into a JavaScript array of objects string.
(define (links->js-array links)
  (string-append "["
    (string-join
      (map (lambda (link)
            (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f}"
              (car (car link))           ;; Source account name
              (cadr (car link))          ;; Source account type
              (car (cadr link))          ;; Destination account name
              (cadr (cadr link))         ;; Destination account type
              (caddr link)))             ;; Flow value
          links)
      ",")
    "]"))

;; This function converts the nodes data structure into a JavaScript object literal string.
(define (nodes->js-array nodes)
  (string-append "{"
    (string-join
      (map (lambda (n)
            (let ((name (car n))
                  (node (cdr n)))
              (format #f "'~a': {'name': '~a', 'type': ~d, 'val': ~,2f, 'inLinks': [~a], 'outLinks': [~a]}"
                name                   ;; Account name
                (noderecord-name node) ;; Account name
                (noderecord-type node) ;; Account type
                (noderecord-val node)  ;; Max of in/out value for sizing
                (string-join
                  (map (lambda (l)
                        (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f}"
                          (linkrecord-source l)  ;; Source account name
                          (linkrecord-s-type l)  ;; Source account type
                          (linkrecord-target l)  ;; Destination account name
                          (linkrecord-t-type l)  ;; Destination account type
                          (linkrecord-value l))) ;; Flow value
                    (noderecord-in-links node))  ;; In-links list of records
                  ",")
                (string-join
                  (map (lambda (l)
                        (format #f "{source: '~a', s_type: ~d, target: '~a', t_type: ~d, value: ~,2f}"
                          (linkrecord-source l)  ;; Source account name
                          (linkrecord-s-type l)  ;; Source account type
                          (linkrecord-target l)  ;; Destination account name
                          (linkrecord-t-type l)  ;; Destination account type
                          (linkrecord-value l))) ;; Flow value
                    (noderecord-out-links node)) ;; Out-links list of records
                  ",")
                )))
          nodes)
      ",")
    "}"))

;; This function converts the levels data structure into a JavaScript object literal string.
(define (levels->js-array levels)
  (string-append "{"
    (string-join
      (map (lambda (lvl)
            (format #f "'~a': ~d" (car lvl) (cdr lvl)))
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
                      (format #f "nodes['~a']" (car n)))
                    col)
                ",")
              "]"))
          cols)
      ",")
    "]"))

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

  try {
    // 4. VERTICAL ALIGNMENT AND SCALING
    var maxColVal = 0;
    var maxColNodes = 0;
    for (var c = 0;
          c < cols.length;
          c++) {
      var col = cols[c];
      var colVal = 0;
      for (var n = 0;
            n < col.length;
            n++) {
        colVal += col[n].val;
      }
      if (colVal > maxColVal) maxColVal = colVal;
      if (col.length > maxColNodes) maxColNodes = col.length;
    }

    var usableHeight = height - (maxColNodes + 1) * nodePadding;
    var scale = maxColVal > 0 ? (usableHeight / maxColVal) : 1;

    // Position nodes inside columns (dynamically centered)
    var colWidth = numCols > 1 ? ((width - nodeWidth) / (numCols - 1)) : width;

    for (var c = 0;
          c < cols.length;
          c++) {
      var col = cols[c];
      col.sort(function(a, b) {
        return b.val - a.val;
      });

      var totalColVal = 0;
      for (var n = 0;
            n < col.length;
            n++) {
        totalColVal += col[n].val;
      }
      var totalColHeight = (totalColVal * scale) + (col.length - 1) * nodePadding;
      var currentY = (height - totalColHeight) / 2;

      for (var n = 0;
            n < col.length;
            n++) {
        var node = col[n];
        node.x0 = c * colWidth;
        node.x1 = node.x0 + nodeWidth;
        node.y0 = currentY;
        node.h = node.val * scale;
        node.y1 = currentY + node.h;

        node.sourceOffset = 0;
        node.targetOffset = 0;

        currentY += node.h + nodePadding;
      }
    }

    // Sort links to minimize crossings
    links.sort(function(a, b) {
      var ya = nodes[a.source].y0;
      var yb = nodes[b.source].y0;
      if (ya !== yb) return ya - yb;
      return nodes[a.target].y0 - nodes[b.target].y0;
    });

    // 5. GENERATE SVG
    var svgParts = [];
    svgParts.push('<svg viewBox=\"0 0 ' + width + ' ' + height + '\" style=\"width: 100%; height: auto; font-family: sans-serif;\">');

    // DRAW LINKS (Bezier S-Curves)
    for (var i = 0; i < links.length; i++) {
      var l = links[i];
      var sNode = nodes[l.source];
      var tNode = nodes[l.target];

      var linkH = l.value * scale;
      var yStart = sNode.y0 + sNode.sourceOffset + (linkH / 2);
      var yEnd = tNode.y0 + tNode.targetOffset + (linkH / 2);

      sNode.sourceOffset += linkH;
      tNode.targetOffset += linkH;

      var x0 = sNode.x1;
      var x1 = tNode.x0;
      var dx = (x1 - x0) / 2;

      var color = getNodeColor(l.s_type);
      var pathData = 'M' + x0 + ',' + yStart + ' C' + (x0 + dx) + ',' + yStart + ' ' + (x1 - dx) + ',' + yEnd + ' ' + x1 + ',' + yEnd;

      svgParts.push('<path d=\"' + pathData + '\" fill=\"none\" stroke=\"' + color + '\" stroke-width=\"' + Math.max(1, linkH) + '\" stroke-opacity=\"0.35\">');
      svgParts.push('  <title>' + l.source + ' &rarr; ' + l.target + ': $' + l.value.toFixed(2) + '</title>');
      svgParts.push('</path>');
    }

    // DRAW NODES & LABELS
    for (var name in nodes) {
      var node = nodes[name];
      var color = getNodeColor(node.type);
      var shortName = name.split(':').pop();

      svgParts.push('<g>');
      svgParts.push('  <rect x=\"' + node.x0 + '\" y=\"' + node.y0 + '\" width=\"' + nodeWidth + '\" height=\"' + Math.max(2, node.h) + '\" fill=\"' + color + '\" stroke=\"#2c3e50\" stroke-width=\"1\" rx=\"2\" ry=\"2\">');
      svgParts.push('    <title>' + name + ': $' + node.val.toFixed(2) + '</title>');
      svgParts.push('  </rect>');

      // Dynamic label placement (Left aligned on left side of chart, right aligned on right side)
      var isLeftHalf = node.x0 < (width / 2);
      var textX = isLeftHalf ? (node.x1 + 8) : (node.x0 - 8);
      var textAnchor = isLeftHalf ? 'start' : 'end';
      var textY = node.y0 + (node.h / 2) + 4;

      svgParts.push('  <text x=\"' + textX + '\" y=\"' + textY + '\" text-anchor=\"' + textAnchor + '\" font-size=\"8\" fill=\"#2c3e50\" font-weight=\"bold\">');
      svgParts.push(     shortName + ' ($' + node.val.toFixed(0) + ')');
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
         (js-links (links->js-array links)))

    (push (format #f "<p>From Date: <b>~a</b></p>\n" (gnc:html-sankey-from-date sankey)))
    (push (format #f "<p>To Date: <b>~a</b></p>\n" (gnc:html-sankey-to-date sankey)))
    (push (format #f "<div id=sankey_chart ~a>\n" (chart-div-style (gnc:html-sankey-height sankey))))
    (push (format #f "  <div id=sankey_message ~a>\n" message-div-style))

    (if (or (string=? js-links "[]") (null? js-links))
      ;; skip the javascript rendering and just show a message if no data
      (begin
        (push "    <h4>No cash flow data found.</h4>\n")
        (push "    <p>Ensure you have selected correct dates and accounts with transactions in Options.</p>\n")
        (push "  </div>\n")
        (push "</div>\n"))
      ;; otherwise render the chart
      (let* ((nodes (populate-nodes links))
             (js-nodes (nodes->js-array nodes))
             (style (gnc:html-sankey-x-axis-style sankey))
             (levels (populate-levels nodes style))
             (js-levels (levels->js-array levels))
             (max-lvl (max-level levels))
             (js-nodes (nodes->js-array nodes))
             (cols (populate-cols max-lvl nodes levels))
             (js-cols (cols->js-array cols)))
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
          (push "  var nodePadding = 18;\n")
          (push "  var nodeWidth = 24;\n\n")
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
