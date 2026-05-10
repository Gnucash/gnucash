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
(use-modules (gnucash json builder))
(use-modules (gnucash report html-utilities))
(use-modules (gnucash report report-utilities))
(use-modules (srfi srfi-9))

(export gnc:html-sankey?)
(export gnc:make-html-sankey)

(export gnc:html-sankey-set-from-date!)
(export gnc:html-sankey-set-to-date!)

(export gnc:html-sankey-set-js-data!)

(export gnc:html-sankey-set-income-color!)
(export gnc:html-sankey-set-expense-color!)
(export gnc:html-sankey-set-asset-color!)
(export gnc:html-sankey-set-liability-color!)
(export gnc:html-sankey-set-equity-color!)
(export gnc:html-sankey-set-fallback-color!)

(export gnc:html-sankey-render)

(define-record-type <html-sankey>
  (make-html-sankey from-date to-date income-color expense-color asset-color liability-color equity-color fallback-color js-data)
  html-sankey?
  (from-date html-sankey-from-date html-sankey-set-from-date!)
  (to-date html-sankey-to-date html-sankey-set-to-date!)
  (income-color html-sankey-income-color html-sankey-set-income-color!)
  (expense-color html-sankey-expense-color html-sankey-set-expense-color!)
  (asset-color html-sankey-asset-color html-sankey-set-asset-color!)
  (liability-color html-sankey-liability-color html-sankey-set-liability-color!)
  (equity-color html-sankey-equity-color html-sankey-set-equity-color!)
  (fallback-color html-sankey-fallback-color html-sankey-set-fallback-color!)
  (js-data html-sankey-js-data html-sankey-set-js-data!))

(define gnc:html-sankey? html-sankey?)

(define gnc:html-sankey-from-date html-sankey-from-date)
(define gnc:html-sankey-set-from-date! html-sankey-set-from-date!)
(define gnc:html-sankey-to-date html-sankey-to-date)
(define gnc:html-sankey-set-to-date! html-sankey-set-to-date!)

(define gnc:html-sankey-js-data html-sankey-js-data)
(define gnc:html-sankey-set-js-data! html-sankey-set-js-data!)

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

(define (gnc:make-html-sankey)
  (make-html-sankey
;  1200                             ;width
;  800                              ;height
  '()                              ;from-date
  '()                              ;to-date
  '()                              ;income-color'
  '()                              ;expense-color'
  '()                              ;asset-color'
  '()                              ;liability-color'
  '()                              ;equity-color'
  '()                              ;fallback-color'
  '()                              ;js-data
  ))

(define div-style "style='width: 100%;
  height: 700px;
  background: #fafafa;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  padding: 20px;
  box-sizing: border-box;'"
  )

;; Dynamic color picker based on GnuCash account type
(define js-node-color "
function getNodeColor(nodeName) {
  var name = nodeName.toLowerCase();
  if (name.indexOf('income') !== -1) return incomeColor;
  if (name.indexOf('expense') !== -1) return expenseColor;
  if (name.indexOf('asset') !== -1) return assetColor;
  if (name.indexOf('liabilit') !== -1) return liabilityColor;
  if (name.indexOf('equity') !== -1) return equityColor;
  return fallbackColor;
}
")

(define js-sankey "var chartDiv = document.getElementById('sankey_chart');

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
}")

(define (gnc:html-sankey-render sankey doc)
  (gnc:format "
<p>From Date: <b>${from-date}</b></p>
<p>To Date: <b>${to-date}</b></p>
<div id=sankey_chart ${div-style}></div>
<script>
(function () {
  var rawData = ${js-data};
  var incomeColor = '${income-color}';
  var expenseColor = '${expense-color}';
  var assetColor = '${asset-color}';
  var liabilityColor = '${liability-color}';
  var equityColor = '${equity-color}';
  var fallbackColor = '${fallback-color}';
  ${js-node-color}
  ${js-sankey}
})();
</script>"
        'div-style div-style
        'from-date (gnc:html-sankey-from-date sankey)
        'to-date (gnc:html-sankey-to-date sankey)
        'income-color (gnc:html-sankey-income-color sankey)
        'expense-color (gnc:html-sankey-expense-color sankey)
        'asset-color (gnc:html-sankey-asset-color sankey)
        'liability-color (gnc:html-sankey-liability-color sankey)
        'equity-color (gnc:html-sankey-equity-color sankey)
        'fallback-color (gnc:html-sankey-fallback-color sankey)
        'js-data (gnc:html-sankey-js-data sankey)
        'js-node-color js-node-color
        'js-sankey js-sankey))
