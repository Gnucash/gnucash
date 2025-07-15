;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-sankey.scm : generate Sankey chart with SVG
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report html-sankey))

(use-modules (gnucash core-utils))
(use-modules (gnucash json builder))            ;for building JSON options
(use-modules (gnucash report html-utilities))
(use-modules (gnucash report report-utilities))
(use-modules (srfi srfi-9))

;; html-sankey.scm

(export gnc:html-sankey?)
(export gnc:make-html-sankey)
(export gnc:html-sankey-set-width!)
(export gnc:html-sankey-set-height!)
(export gnc:html-sankey-set-nodes!)
(export gnc:html-sankey-set-links!)
(export gnc:html-sankey-render)

(define-record-type <html-sankey>
  (make-html-sankey width height nodes links)
  html-sankey?
  (width html-sankey-width html-sankey-set-width!)
  (height html-sankey-height html-sankey-set-height!)
  (nodes html-sankey-nodes html-sankey-set-nodes!)
  (links html-sankey-links html-sankey-set-links!))

(define gnc:html-sankey? html-sankey?)
(define gnc:html-sankey-width html-sankey-width)
(define gnc:html-sankey-set-width! html-sankey-set-width!)
(define gnc:html-sankey-height html-sankey-height)
(define gnc:html-sankey-set-height! html-sankey-set-height!)
(define gnc:html-sankey-nodes html-sankey-nodes)
(define gnc:html-sankey-set-nodes! html-sankey-set-nodes!)
(define gnc:html-sankey-links html-sankey-links)
(define gnc:html-sankey-set-links! html-sankey-set-links!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-sankey> class
;;  generate the <object> form for an html sankey.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (gnc:make-html-sankey)
  (make-html-sankey
   1200                             ;width
   800                              ;height
   '()                              ;nodes
   '()                              ;links
   ))

(define sankey-style "<style>
  body { font-family: sans-serif; }
  .node rect { fill: steelblue; stroke: #000; cursor: move; }
  .node text { pointer-events: none; fill: #000; font-size: 12px; }
  .link { fill: none; stroke: #000; stroke-opacity: 0.2; }
  .link:hover { stroke-opacity: 0.5; }
</style>")

(define js-sankey "var width = +svg.attr('width');
var height = +svg.attr('height');
var sankey = d3.sankey()
  .nodeWidth(20)
  .nodePadding(10)
  .size([width, height]);

var path = sankey.link();

sankey.nodes(data.nodes)
      .links(data.links)
      .layout(32);

var link = svg.append('g').selectAll('.link')
  .data(data.links)
  .enter().append('path')
  .attr('class', 'link')
  .attr('d', path)
  .style('stroke-width', function(d) { return Math.max(1, d.dy); })
  .sort(function(a, b) { return b.dy - a.dy; });

link.append('title')
  .text(function(d) { return d.source.name + ' → ' + d.target.name + '\\n' + d.value; });

var node = svg.append('g').selectAll('.node')
  .data(data.nodes)
  .enter().append('g')
  .attr('class', 'node')
  .attr('transform', function(d) { return 'translate(' + d.x + ',' + d.y + ')'; })
  .call(d3.behavior.drag()
    .origin(function(d) { return d; })
    .on('dragstart', function() { this.parentNode.appendChild(this); })
    .on('drag', dragmove));

node.append('rect')
  .attr('height', function(d) { return d.dy; })
  .attr('width', sankey.nodeWidth())
  .style('fill', 'steelblue')
  .style('stroke', '#000')
  .append('title')
  .text(function(d) { return d.name + '\\n' + d.value; });

node.append('text')
  .attr('x', -6)
  .attr('y', function(d) { return d.dy / 2; })
  .attr('dy', '.35em')
  .attr('text-anchor', 'end')
  .text(function(d) { return d.name; })
  .filter(function(d) { return d.x < width / 2; })
  .attr('x', 6 + sankey.nodeWidth())
  .attr('text-anchor', 'start');

function dragmove(d) {
  d3.select(this).attr('transform',
    'translate(' + d.x + ',' + (d.y = Math.max(0, Math.min(height - d.dy, d3.event.y))) + ')');
  sankey.relayout();
  link.attr('d', path);
}
")

(define (gnc:html-sankey-render sankey doc)
  (gnc:format "${sankey-style}${d3-path}${d3-sankey-path}
<svg id='${id}' width='${width}' height='${height}'></svg>
<script>
var data = ${data};
var svg = d3.select('#${id}');
${js-sankey}
</script>"
              'sankey-style sankey-style
              'd3-path (gnc:html-js-include "d3-3/d3.v3.js")
              'd3-sankey-path (gnc:html-js-include "d3-sankey/d3-sankey.js")
              'id (gensym "sankey")
              'width (html-sankey-width sankey)
              'height (html-sankey-height sankey)
              'data (scm->json-string
                     `((nodes . ,(list->vector (html-sankey-nodes sankey)))
                       (links . ,(list->vector (html-sankey-links sankey)))))
              'js-sankey js-sankey))

