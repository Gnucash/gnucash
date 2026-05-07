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
(define-module (gnucash report standard sankey-report))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))
(use-modules (ice-9 format))

;; For debugging during development, enable backtraces to get more detailed error information
;; in the GnuCash error logs when something goes wrong.
(debug-enable 'backtrace)

(define report-title (N_ "Cash Flow Sankey"))

(define optinterval-name (N_ "Date Interval"))
(define optinterval-help (N_ "The date interval for the report."))
(define optinterval-from-date (N_ "Start Date"))
(define optinterval-to-date (N_ "End Date"))

(define optaccountlist-name (N_ "Account List"))
(define optaccountlist-help (N_ "The accounts to include in the report."))

;; Minimum incoming flow value to include in the Sankey diagram
;; i.e., show whole dollars only, ignore tiny flows that add noise
(define optminimum-name (N_ "Minimum Flow Value"))
(define optminimum-help (N_ "The minimum incoming flow value to include in the report."))
(define optminimum-default 1.00)

;; Node colors for different account types, with defaults chosen for good contrast and aesthetics
;; Users can customize these in the report options to fit their preferences or color schemes.
;; GnuCash expects just the hex digits so we'll add the # back when generating the CSS for the report.
(define optnodecolor-income-name (N_ "Income Node Color"))
(define optnodecolor-income-help (N_ "The color used for income nodes in the Sankey diagram."))
(define optnodecolor-income-default "27ae60") ;; Forest Green #27ae60
(define optnodecolor-expense-name (N_ "Expense Node Color"))
(define optnodecolor-expense-help (N_ "The color used for expense nodes in the Sankey diagram."))
(define optnodecolor-expense-default "c0392b") ;; Rich Red #c0392b
(define optnodecolor-asset-name (N_ "Asset Node Color"))
(define optnodecolor-asset-help (N_ "The color used for asset nodes in the Sankey diagram."))
(define optnodecolor-asset-default "2980b9") ;; Ice Blue #2980b9
(define optnodecolor-liability-name (N_ "Liability Node Color"))
(define optnodecolor-liability-help (N_ "The color used for liability nodes in the Sankey diagram."))
(define optnodecolor-liability-default "fff700") ;; Lemon #fff700
(define optnodecolor-equity-name (N_ "Equity Node Color"))
(define optnodecolor-equity-help (N_ "The color used for equity nodes in the Sankey diagram."))
(define optnodecolor-equity-default "8e44ad") ;; Plum Purple #8e44ad
(define optnodecolor-fallback-name (N_ "Fallback Node Color"))
(define optnodecolor-fallback-help (N_ "The color used for nodes that do not fit into other categories."))
(define optnodecolor-fallback-default "7f8c8d") ;; Slate Grey #7f8c8d

(define accounts-page    gnc:pagename-accounts)
(define general-page     gnc:pagename-general)
(define colors-page      (N_ "Node Colors"))

(define (extract-sankey-links account-list start-date end-date flow-minimum)
  (let ((links '()))
    (for-each 
      (lambda (account)
        (let ((splits (xaccAccountGetSplitList account)))
          (for-each 
            (lambda (src-split)
              (let* ((trans (xaccSplitGetParent src-split))
                     (date (gnc:time64-start-day-time (gnc:date-option-absolute-time (xaccTransGetDate trans))))
                     (amount (gnc-numeric-to-double (xaccSplitGetAmount src-split))))

                ;; Compare dates against the discrete start and end variables
                (if (and (>= date start-date) (<= date end-date) (< amount 0))
                    (let* ((all-splits (xaccTransGetSplitList trans))
                           (dest-splits (filter (lambda (s) (> (gnc-numeric-to-double (xaccSplitGetAmount s)) 0)) all-splits))
                           (total-dest-val (apply + (map (lambda (s) (gnc-numeric-to-double (xaccSplitGetAmount s))) dest-splits))))

                      (if (>= total-dest-val flow-minimum)
                        (for-each (lambda (dest-split)
                                    (let* ((dest-acc (xaccSplitGetAccount dest-split))
                                           (dest-val (gnc-numeric-to-double (xaccSplitGetAmount dest-split)))
                                           (flow-val (* (abs amount) (/ dest-val total-dest-val))))
                                      (set! links (cons (list (gnc-account-get-full-name account)
                                                              (gnc-account-get-full-name dest-acc)
                                                              flow-val)
                                                        links))))
                                  dest-splits))))))
            splits)))
      account-list)
    links))

(define (links->js-array links)
  (string-append "["
    (string-join 
      (map (lambda (link)
            (format #f "['~a', '~a', ~f]" (car link) (cadr link) (caddr link)))
          links)
      ",")
    "]"))

(define (sankey-options-generator)
  (let* ((options (gnc-new-optiondb)))

    (gnc:options-add-date-interval! options
      general-page                              ;; Tab
      optinterval-from-date optinterval-to-date ;; Option Names
      "b")                                      ;; Sorting key

    (gnc-register-number-range-option options
      general-page        ;; Tab
      optminimum-name     ;; Option Name
      "a"                 ;; Sorting key
      optminimum-help     ;; Help text
      optminimum-default  ;; default
      0.0                 ;; lower bound
      10000.0             ;; upper bound
      0.01)               ;; step size

    (gnc-register-account-list-option options
      accounts-page       ;; Tab
      optaccountlist-name ;; Option Name
      "a"                 ;; Sorting key
      optaccountlist-help ;; Help text
      (gnc:filter-accountlist-type
         ;; select, by default, only income and expense accounts
         (list ACCT-TYPE-EXPENSE ACCT-TYPE-INCOME)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-income-name        ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-income-help        ;; Help text
      optnodecolor-income-default)    ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-expense-name       ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-expense-help       ;; Help text
      optnodecolor-expense-default)   ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-asset-name         ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-asset-help         ;; Help text
      optnodecolor-asset-default)     ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-liability-name     ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-liability-help     ;; Help text
      optnodecolor-liability-default) ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-equity-name        ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-equity-help        ;; Help text
      optnodecolor-equity-default)    ;; Default value

    (gnc-register-color-option options
      colors-page                     ;; Tab
      optnodecolor-fallback-name      ;; Option Name
      "c"                             ;; Sorting key
      optnodecolor-fallback-help      ;; Help text
      optnodecolor-fallback-default)  ;; Default value

    options))

(define (sankey-renderer report-obj)
  ;; Helper function for looking up option values.
  (define (op-value section name)
    (let ((options (gnc:report-options report-obj)))
      (gnc-optiondb-lookup-value options section name)))

  (let* (
    ;; Fetching the option values
    (options (gnc:report-options report-obj))
    (from-date (gnc:date-option-absolute-time (op-value general-page optinterval-from-date)))
    (to-date (gnc:date-option-absolute-time (op-value general-page optinterval-to-date)))
    (interval (op-value general-page optinterval-name))
    (flow-minimum (op-value general-page optminimum-name))
    (selected-accounts (op-value accounts-page optaccountlist-name))

    ;; If no accounts were explicitly selected, use all accounts in the book as the default
    (accounts (if (null? selected-accounts)
      (gnc-account-get-descendants-sorted (gnc-book-get-root-account (gnc-get-current-book)))
        selected-accounts))

    ;; Prepare colors, converting from hex strings to CSS format (e.g., "27ae60" -> "#27ae60")
    (income-color (format #f "#~a" (op-value colors-page optnodecolor-income-name)))
    (expense-color (format #f "#~a" (op-value colors-page optnodecolor-expense-name)))
    (asset-color (format #f "#~a" (op-value colors-page optnodecolor-asset-name)))
    (liability-color (format #f "#~a" (op-value colors-page optnodecolor-liability-name)))
    (equity-color (format #f "#~a" (op-value colors-page optnodecolor-equity-name)))
    (fallback-color (format #f "#~a" (op-value colors-page optnodecolor-fallback-name)))

    ;; Format the from and to dates as strings for display in the report
    ;; as well as convert them to time64 values for data extraction/comparison
    (from-date-string (gnc-print-time64 from-date "%x"))
    (to-date-string (gnc-print-time64 to-date "%x"))
    (from-date-t64 (gnc:time64-start-day-time from-date))
    (to-date-t64 (gnc:time64-end-day-time to-date))

    ;; Extract the sankey links based on the selected accounts and date range,
    ;; then convert to a JavaScript array format for embedding in the HTML/JS
    (data-links (extract-sankey-links accounts from-date-t64 to-date-t64 flow-minimum))
    (js-data (links->js-array data-links))
    ;;(js-data "[]") ;; Placeholder until we can get the data extraction working

    ;; Create the report object that will hold the HTML content
    (report (gnc:make-html-document)))

    ;; Now we construct the HTML report, embedding the JavaScript and data for the Sankey diagram.
    ;; The JS code will run in the context of the report's HTML document
    (gnc:html-document-set-title! report report-title)
    (gnc:html-document-add-object! 
      report
      (gnc:make-html-text
        (gnc:html-markup-p
          (gnc:html-markup/format
          (G_ "From Date: ~a")
          (gnc:html-markup-b from-date-string)))

        (gnc:html-markup-p
          (gnc:html-markup/format
          (G_ "To Date: ~a")
          (gnc:html-markup-b to-date-string)))

        ;; For debugging: show the raw JS data array in the report to verify correct extraction and formatting
        ;; (gnc:html-markup/format
        ;;   (G_ "~a") js-data)

        (gnc:html-markup/format (G_ "
<div id='sankey_chart' style='width: 100%; height: 700px; background: #fafafa; border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; box-sizing: border-box;'></div>

<script>
  (function() {
    var rawData = ~a;
    var chartDiv = document.getElementById('sankey_chart');

    if (!rawData || rawData.length === 0) {
        chartDiv.innerHTML = '<div style=\"text-align: center; padding-top: 200px; font-family: sans-serif; color: #555;\"><h3>No cash flow data found.</h3><p>Ensure you have selected correct dates and accounts with transactions in Options.</p></div>';
        return;
    }

    try {
      // 1. AGGREGATE DUPLICATE ENTRIES
      var aggregated = {};
      for (var i = 0; i < rawData.length; i++) {
        var row = rawData[i];
        var s = row[0];
        var t = row[1];
        var v = parseFloat(row[2]) || 0;
        if (v <= 0) continue;
        
        var key = s + ' -> ' + t;
        if (!aggregated[key]) {
          aggregated[key] = { source: s, target: t, value: 0 };
        }
        aggregated[key].value += v;
      }

      var links = [];
      for (var k in aggregated) {
        links.push(aggregated[k]);
      }

      if (links.length === 0) {
        chartDiv.innerHTML = '<h3 style=\"text-align: center; padding-top: 200px; font-family: sans-serif;\">No positive flows to display.</h3>';
        return;
      }

      // 2. DISCOVER UNIQUE NODES & ACCUMULATE TOTALS
      var nodes = {};
      for (var i = 0; i < links.length; i++) {
        var l = links[i];
        if (!nodes[l.source]) {
          nodes[l.source] = { name: l.source, inVal: 0, outVal: 0, inLinks: [], outLinks: [] };
        }
        if (!nodes[l.target]) {
          nodes[l.target] = { name: l.target, inVal: 0, outVal: 0, inLinks: [], outLinks: [] };
        }
        nodes[l.source].outVal += l.value;
        nodes[l.target].inVal += l.value;
        nodes[l.source].outLinks.push(l);
        nodes[l.target].inLinks.push(l);
      }

      for (var name in nodes) {
        var n = nodes[name];
        n.val = Math.max(n.inVal, n.outVal);
      }

      // 3. TOPOLOGICAL LEVELING (X-AXIS COLUMNS)
      var levels = {};
      function getLevel(name) {
        if (levels[name] !== undefined) return levels[name];
        var node = nodes[name];
        if (node.inLinks.length === 0) {
          levels[name] = 0;
          return 0;
        }
        var maxParentLvl = 0;
        for (var j = 0; j < node.inLinks.length; j++) {
          var pName = node.inLinks[j].source;
          var pLvl = getLevel(pName);
          if (pLvl > maxParentLvl) maxParentLvl = pLvl;
        }
        levels[name] = maxParentLvl + 1;
        return levels[name];
      }

      for (var name in nodes) {
        getLevel(name);
      }

      // Group nodes by levels
      var maxLevel = 0;
      for (var name in levels) {
        if (levels[name] > maxLevel) maxLevel = levels[name];
      }

      var cols = [];
      for (var l = 0; l <= maxLevel; l++) {
        cols.push([]);
      }
      for (var name in nodes) {
        var lvl = levels[name];
        cols[lvl].push(nodes[name]);
      }

      cols = cols.filter(function(col) { return col.length > 0; });
      var numCols = cols.length;

      // 4. VERTICAL ALIGNMENT AND SCALING
      var width = 1000;
      var height = 500;
      var nodePadding = 18;
      var nodeWidth = 24;

      var maxColVal = 0;
      var maxColNodes = 0;
      for (var c = 0; c < cols.length; c++) {
        var col = cols[c];
        var colVal = 0;
        for (var n = 0; n < col.length; n++) {
          colVal += col[n].val;
        }
        if (colVal > maxColVal) maxColVal = colVal;
        if (col.length > maxColNodes) maxColNodes = col.length;
      }

      var usableHeight = height - (maxColNodes + 1) * nodePadding;
      var scale = maxColVal > 0 ? (usableHeight / maxColVal) : 1;

      // Position nodes inside columns (dynamically centered)
      var colWidth = numCols > 1 ? ((width - nodeWidth) / (numCols - 1)) : width;

      for (var c = 0; c < cols.length; c++) {
        var col = cols[c];
        col.sort(function(a, b) { return b.val - a.val; });

        var totalColVal = 0;
        for (var n = 0; n < col.length; n++) {
          totalColVal += col[n].val;
        }
        var totalColHeight = (totalColVal * scale) + (col.length - 1) * nodePadding;
        var currentY = (height - totalColHeight) / 2;

        for (var n = 0; n < col.length; n++) {
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

      // Dynamic color picker based on GnuCash account type
      function getNodeColor(nodeName) {
        var name = nodeName.toLowerCase();
        if (name.indexOf('income') !== -1) return '~a';
        if (name.indexOf('expense') !== -1) return '~a';
        if (name.indexOf('asset') !== -1) return '~a';
        if (name.indexOf('liabilit') !== -1) return '~a';
        if (name.indexOf('equity') !== -1) return '~a';
        return '~a';
      }

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

        var color = getNodeColor(l.source);
        var pathData = 'M' + x0 + ',' + yStart + ' C' + (x0 + dx) + ',' + yStart + ' ' + (x1 - dx) + ',' + yEnd + ' ' + x1 + ',' + yEnd;
        
        svgParts.push('<path d=\"' + pathData + '\" fill=\"none\" stroke=\"' + color + '\" stroke-width=\"' + Math.max(1, linkH) + '\" stroke-opacity=\"0.35\">');
        svgParts.push('  <title>' + l.source + ' &rarr; ' + l.target + ': $' + l.value.toFixed(2) + '</title>');
        svgParts.push('</path>');
      }

      // DRAW NODES & LABELS
      for (var name in nodes) {
        var node = nodes[name];
        var color = getNodeColor(name);
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
      chartDiv.innerHTML = '<div style=\"color:red; padding:10px; font-family:sans-serif;\"><h4>Sankey Visualizer Error</h4><pre>' + err.message + '</pre></div>';
    }
  })();
</script>
") js-data income-color expense-color asset-color liability-color equity-color fallback-color)
      ))
  report))

;; --- 4. REGISTRATION ---
(gnc:define-report
 'version 1
 'name report-title
 'report-guid "8374f6b5434442679347f43cb08d2092"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator sankey-options-generator
 'renderer sankey-renderer)
