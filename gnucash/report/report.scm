;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  report.scm
;;  module definition for the report system code
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(define-module (gnucash report))
(use-modules (gnucash utilities)) 
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-19))
(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash gnome-utils))
(use-modules (gnucash html))

;; commodity-utilities.scm
(export gnc:get-match-commodity-splits)
(export gnc:get-match-commodity-splits-sorted)
(export gnc:get-all-commodity-splits )
(export gnc:exchange-by-euro-numeric)
(export gnc:get-commodity-totalavg-prices)
(export gnc:get-commoditylist-totalavg-prices)
(export gnc:get-commodity-inst-prices)
(export gnc:pricelist-price-find-nearest)
(export gnc:pricealist-lookup-nearest-in-time)
(export gnc:resolve-unknown-comm)
(export gnc:get-exchange-totals)
(export gnc:get-exchange-cost-totals)
(export gnc:make-exchange-alist)
(export gnc:make-exchange-cost-alist)
(export gnc:exchange-by-euro)
(export gnc:exchange-if-same)
(export gnc:make-exchange-function)
(export gnc:exchange-by-pricedb-latest )
(export gnc:exchange-by-pricedb-nearest)
(export gnc:exchange-by-pricealist-nearest)
(export gnc:case-exchange-fn)
(export gnc:case-exchange-time-fn)
(export gnc:sum-collector-commodity)

;; options-utilities.scm

(export gnc:options-add-report-date!)
(export gnc:options-add-date-interval!)
(export gnc:options-add-interval-choice!)
(export gnc:options-add-account-levels!)
(export gnc:options-add-account-selection!)
(export gnc:options-add-currency!)
(export gnc:options-add-price-source!)
(export gnc:options-add-plot-size!)
(export gnc:options-add-marker-choice!)
(export gnc:options-add-sort-method!)
(export gnc:options-add-subtotal-view!)

;; html-fonts.scm

(export register-font-options)
(export add-css-information-to-doc)

;; html-utilities.scm

(export gnc:html-make-empty-cell)
(export gnc:html-make-empty-cells)
(export gnc:account-anchor-text)
(export gnc:split-anchor-text)
(export gnc:transaction-anchor-text)
(export gnc:report-anchor-text)
(export gnc:make-report-anchor)
(export gnc:html-account-anchor)
(export gnc:html-split-anchor)
(export gnc:html-transaction-anchor)
(export gnc:html-price-anchor)
(export gnc:customer-anchor-text)
(export gnc:job-anchor-text)
(export gnc:vendor-anchor-text)
(export gnc:invoice-anchor-text)
(export gnc:owner-anchor-text)
(export gnc:owner-report-text)
(export gnc:assign-colors)
(export gnc:html-table-append-ruler!)
(export gnc:html-make-exchangerates)
(export gnc:html-render-options-changed)
(export gnc:html-make-generic-warning)
(export gnc:html-make-no-account-warning)
(export gnc:html-make-generic-budget-warning)
(export gnc:html-make-generic-options-warning)
(export gnc:html-make-generic-simple-warning)
(export gnc:html-make-empty-data-warning)
(export gnc:html-make-options-link)
(export gnc:html-js-include)
(export gnc:html-css-include)

;; report-core.scm
(export gnc:menuname-reports)
(export gnc:menuname-asset-liability)
(export gnc:menuname-income-expense)
(export gnc:menuname-budget)
(export gnc:menuname-taxes)
(export gnc:menuname-example)
(export gnc:menuname-multicolumn)
(export gnc:menuname-experimental)
(export gnc:menuname-custom)
(export gnc:menuname-business-reports)
(export gnc:pagename-general)
(export gnc:pagename-accounts)
(export gnc:pagename-display)
(export gnc:optname-reportname)
(export gnc:optname-invoice-number)

(export gnc:define-report)
(export <report>)
(export gnc:report-template-new-options/report-guid)
(export gnc:report-template-menu-name/report-guid)
(export gnc:report-template-renderer/report-guid)
(export gnc:report-template-new-options)
(export gnc:report-template-version)
(export gnc:report-template-name)
(export gnc:report-template-report-guid)
(export gnc:report-template-set-report-guid!)
(export gnc:report-template-options-generator)
(export gnc:report-template-options-cleanup-cb)
(export gnc:report-template-options-changed-cb)
(export gnc:report-template-renderer)
(export gnc:report-template-in-menu?)
(export gnc:report-template-menu-path)
(export gnc:report-template-menu-name)
(export gnc:report-template-menu-tip)
(export gnc:report-template-export-types)
(export gnc:report-template-export-thunk)
(export gnc:report-template-has-unique-name?)
(export gnc:report-type)
(export gnc:report-set-type!)
(export gnc:report-id)
(export gnc:report-set-id!)
(export gnc:report-options)
(export gnc:report-set-options!)
(export gnc:report-needs-save?)
(export gnc:report-set-needs-save?!)
(export gnc:report-dirty?)
(export gnc:report-set-dirty?!)
(export gnc:report-editor-widget)
(export gnc:report-set-editor-widget!)
(export gnc:report-ctext)
(export gnc:report-set-ctext!)
(export gnc:make-report)
(export gnc:restore-report-by-guid)
(export gnc:restore-report-by-guid-with-custom-template)
(export gnc:make-report-options)
(export gnc:report-export-types)
(export gnc:report-export-thunk)
(export gnc:report-menu-name)
(export gnc:report-name)
(export gnc:report-stylesheet)
(export gnc:report-set-stylesheet!)
(export gnc:all-report-template-guids)
(export gnc:custom-report-template-guids)
(export gnc:delete-report)
(export gnc:rename-report)
(export gnc:find-report-template)
(export gnc:report-serialize)
(export gnc:report-to-template-new)
(export gnc:report-to-template-update)
(export gnc:report-render-html)
(export gnc:report-run)
(export gnc:report-templates-for-each)
(export gnc:report-embedded-list)
(export gnc:report-template-is-custom/template-guid?)
(export gnc:is-custom-report-type)

;; html-barchart.scm

(export <html-barchart>)
(export gnc:html-barchart? )
(export gnc:make-html-barchart-internal)
(export gnc:make-html-barchart)
(export gnc:html-barchart-data)
(export gnc:html-barchart-set-data!)
(export gnc:html-barchart-width)
(export gnc:html-barchart-set-width!)
(export gnc:html-barchart-height)
(export gnc:html-barchart-set-height!)
(export gnc:html-barchart-x-axis-label)
(export gnc:html-barchart-set-x-axis-label!)
(export gnc:html-barchart-y-axis-label)
(export gnc:html-barchart-set-y-axis-label!)
(export gnc:html-barchart-row-labels)
(export gnc:html-barchart-set-row-labels!)
(export gnc:html-barchart-row-labels-rotated?)
(export gnc:html-barchart-set-row-labels-rotated?!)
(export gnc:html-barchart-stacked?)
(export gnc:html-barchart-set-stacked?!)
(export gnc:html-barchart-col-labels)
(export gnc:html-barchart-set-col-labels!)
(export gnc:html-barchart-col-colors)
(export gnc:html-barchart-set-col-colors!)
(export gnc:html-barchart-legend-reversed?)
(export gnc:html-barchart-set-legend-reversed?!)
(export gnc:html-barchart-title)
(export gnc:html-barchart-set-title!)
(export gnc:html-barchart-subtitle)
(export gnc:html-barchart-set-subtitle!)
(export gnc:html-barchart-button-1-bar-urls)
(export gnc:html-barchart-set-button-1-bar-urls!)
(export gnc:html-barchart-button-2-bar-urls)
(export gnc:html-barchart-set-button-2-bar-urls!)
(export gnc:html-barchart-button-3-bar-urls)
(export gnc:html-barchart-set-button-3-bar-urls!)
(export gnc:html-barchart-button-1-legend-urls)
(export gnc:html-barchart-set-button-1-legend-urls!)
(export gnc:html-barchart-button-2-legend-urls)
(export gnc:html-barchart-set-button-2-legend-urls!)
(export gnc:html-barchart-button-3-legend-urls)
(export gnc:html-barchart-set-button-3-legend-urls!)
(export gnc:html-barchart-append-row!)
(export gnc:html-barchart-prepend-row!)
(export gnc:html-barchart-append-column!)
(export gnc:not-all-zeros)
(export gnc:html-barchart-prepend-column!)
(export gnc:html-barchart-render barchart)

;; html-document.scm

(export <html-document>)
(export gnc:html-document?)
(export gnc:make-html-document-internal)
(export gnc:make-html-document)
(export gnc:html-document-set-title!)
(export gnc:html-document-title)
(export gnc:html-document-set-headline!)
(export gnc:html-document-headline)
(export gnc:html-document-set-style-text!)
(export gnc:html-document-style-text)
(export gnc:html-document-set-style-sheet!)
(export gnc:html-document-style-sheet)
(export gnc:html-document-set-style-stack!)
(export gnc:html-document-style-stack)
(export gnc:html-document-set-style-internal!)
(export gnc:html-document-style)
(export gnc:html-document-set-objects!)
(export gnc:html-document-objects)
(export gnc:html-document?)
(export gnc:html-document-set-style!)
(export gnc:html-document-tree-collapse)
(export gnc:html-document-render)
(export gnc:html-document-push-style)
(export gnc:html-document-pop-style)
(export gnc:html-document-add-object!)
(export gnc:html-document-append-objects!)
(export gnc:html-document-fetch-markup-style)
(export gnc:html-document-fetch-data-style)
(export gnc:html-document-markup-start)
(export gnc:html-document-markup-end)
(export gnc:html-document-render-data)
(export <html-object>)
(export gnc:html-object?)
(export gnc:make-html-object-internal)
(export gnc:make-html-object)
(export gnc:html-object-renderer)
(export gnc:html-object-set-renderer!)
(export gnc:html-object-data)
(export gnc:html-object-set-data!)
(export gnc:html-object-render)

;; html-piechart.scm

(export <html-piechart>)
(export gnc:html-piechart?)
(export gnc:make-html-piechart-internal)
(export gnc:make-html-piechart)
(export gnc:html-piechart-data)
(export gnc:html-piechart-set-data!)
(export gnc:html-piechart-width)
(export gnc:html-piechart-set-width!)
(export gnc:html-piechart-height)
(export gnc:html-piechart-set-height!)
(export gnc:html-piechart-labels)
(export gnc:html-piechart-set-labels!)
(export gnc:html-piechart-colors)
(export gnc:html-piechart-set-colors!)
(export gnc:html-piechart-title)
(export gnc:html-piechart-set-title!)
(export gnc:html-piechart-subtitle)
(export gnc:html-piechart-set-subtitle!)
(export gnc:html-piechart-button-1-slice-urls)
(export gnc:html-piechart-set-button-1-slice-urls!)
(export gnc:html-piechart-button-2-slice-urls)
(export gnc:html-piechart-set-button-2-slice-urls!)
(export gnc:html-piechart-button-3-slice-urls)
(export gnc:html-piechart-set-button-3-slice-urls!)
(export gnc:html-piechart-button-1-legend-urls)
(export gnc:html-piechart-set-button-1-legend-urls!)
(export gnc:html-piechart-button-2-legend-urls)
(export gnc:html-piechart-set-button-2-legend-urls!)
(export gnc:html-piechart-button-3-legend-urls)
(export gnc:html-piechart-set-button-3-legend-urls!)
(export gnc:html-piechart-render)

;; html-scatter.scm

(export <html-scatter>)
(export gnc:html-scatter?)
(export gnc:make-html-scatter-internal)
(export gnc:make-html-scatter)
(export gnc:html-scatter-width)
(export gnc:html-scatter-set-width!)
(export gnc:html-scatter-height)
(export gnc:html-scatter-set-height!)
(export gnc:html-scatter-title)
(export gnc:html-scatter-set-title!)
(export gnc:html-scatter-subtitle)
(export gnc:html-scatter-set-subtitle!)
(export gnc:html-scatter-x-axis-label)
(export gnc:html-scatter-set-x-axis-label!)
(export gnc:html-scatter-y-axis-label)
(export gnc:html-scatter-set-y-axis-label!)
(export gnc:html-scatter-data)
(export gnc:html-scatter-set-data!)
(export gnc:html-scatter-marker)
(export gnc:html-scatter-set-marker!)
(export gnc:html-scatter-markercolor)
(export gnc:html-scatter-set-markercolor!)
(export gnc:html-scatter-add-datapoint!)
(export gnc:html-scatter-render)

;; html-linechart.scm

(export <html-linechart>)
(export gnc:html-linechart? )
(export gnc:make-html-linechart-internal)
(export gnc:make-html-linechart)
(export gnc:html-linechart-data)
(export gnc:html-linechart-set-data!)
(export gnc:html-linechart-width)
(export gnc:html-linechart-set-width!)
(export gnc:html-linechart-height)
(export gnc:html-linechart-set-height!)
(export gnc:html-linechart-x-axis-label)
(export gnc:html-linechart-set-x-axis-label!)
(export gnc:html-linechart-y-axis-label)
(export gnc:html-linechart-set-y-axis-label!)
(export gnc:html-linechart-row-labels)
(export gnc:html-linechart-set-row-labels!)
(export gnc:html-linechart-row-labels-rotated?)
(export gnc:html-linechart-set-row-labels-rotated?!)
(export gnc:html-linechart-stacked?)
(export gnc:html-linechart-set-stacked?!)
(export gnc:html-linechart-markers?)
(export gnc:html-linechart-set-markers?!)
(export gnc:html-linechart-major-grid?)
(export gnc:html-linechart-set-major-grid?!)
(export gnc:html-linechart-minor-grid?)
(export gnc:html-linechart-set-minor-grid?!)
(export gnc:html-linechart-col-labels)
(export gnc:html-linechart-set-col-labels!)
(export gnc:html-linechart-col-colors)
(export gnc:html-linechart-set-col-colors!)
(export gnc:html-linechart-legend-reversed?)
(export gnc:html-linechart-set-legend-reversed?!)
(export gnc:html-linechart-title)
(export gnc:html-linechart-set-title!)
(export gnc:html-linechart-subtitle)
(export gnc:html-linechart-set-subtitle!)
(export gnc:html-linechart-button-1-line-urls)
(export gnc:html-linechart-set-button-1-line-urls!)
(export gnc:html-linechart-button-2-line-urls)
(export gnc:html-linechart-set-button-2-line-urls!)
(export gnc:html-linechart-button-3-line-urls)
(export gnc:html-linechart-set-button-3-line-urls!)
(export gnc:html-linechart-button-1-legend-urls)
(export gnc:html-linechart-set-button-1-legend-urls!)
(export gnc:html-linechart-button-2-legend-urls)
(export gnc:html-linechart-set-button-2-legend-urls!)
(export gnc:html-linechart-button-3-legend-urls)
(export gnc:html-linechart-set-button-3-legend-urls!)
(export gnc:html-linechart-append-row!)
(export gnc:html-linechart-prepend-row!)
(export gnc:html-linechart-append-column!)
(export gnc:html-linechart-prepend-column!)
(export gnc:html-linechart-render linechart)
(export gnc:html-linechart-set-line-width!)
(export gnc:html-linechart-line-width)

;; html-style-info.scm

(export <html-markup-style-info>)
(export gnc:html-markup-style-info?)
(export gnc:make-html-markup-style-info-internal)
(export gnc:make-html-markup-style-info)
(export gnc:html-markup-style-info-set!)
(export gnc:html-markup-style-info-tag)
(export gnc:html-markup-style-info-set-tag!)
(export gnc:html-markup-style-info-attributes)
(export gnc:html-markup-style-info-set-attributes!)
(export gnc:html-markup-style-info-inheritable?)
(export gnc:html-markup-style-info-set-inheritable?!)
(export gnc:html-markup-style-info-set-attribute!)
(export gnc:html-markup-style-info-merge)
(export gnc:html-style-info-merge)
(export gnc:html-data-style-info-merge)
(export <html-data-style-info>)
(export gnc:html-data-style-info?)
(export gnc:make-html-data-style-info-internal)
(export gnc:make-html-data-style-info)
(export gnc:html-data-style-info?)
(export gnc:html-data-style-info-renderer)
(export gnc:html-data-style-info-set-renderer!)
(export gnc:html-data-style-info-data)
(export gnc:html-data-style-info-set-data!)
(export gnc:html-data-style-info-inheritable?)
(export gnc:html-data-style-info-set-inheritable?!)
(export gnc:default-html-string-renderer)
(export gnc:default-html-gnc-numeric-renderer)
(export gnc:default-html-gnc-monetary-renderer)
(export gnc:default-html-number-renderer)
(export <html-style-table>)
(export gnc:html-style-table?)
(export gnc:make-html-style-table-internal)
(export gnc:make-html-style-table)
(export gnc:html-style-table-primary)
(export gnc:html-style-table-compiled)
(export gnc:html-style-table-set-compiled!)
(export gnc:html-style-table-inheritable)
(export gnc:html-style-table-set-inheritable!)
(export gnc:html-style-table-compiled?)
(export gnc:html-style-table-compile)
(export gnc:html-style-table-uncompile)
(export gnc:html-style-table-fetch)
(export gnc:html-style-table-set!)

;; html-style-sheet.scm

(export <html-style-sheet-template>)
(export gnc:html-style-sheet-template?)
(export gnc:html-style-sheet-template-version)
(export gnc:html-style-sheet-template-set-version!)
(export gnc:html-style-sheet-template-name)
(export gnc:html-style-sheet-template-set-name!)
(export gnc:html-style-sheet-template-options-generator)
(export gnc:html-style-sheet-template-set-options-generator!)
(export gnc:html-style-sheet-template-renderer)
(export gnc:html-style-sheet-template-set-renderer!)
(export gnc:html-style-sheet-template-find)
(export gnc:define-html-style-sheet)
(export <html-style-sheet>)
(export gnc:html-style-sheet?)
(export gnc:html-style-sheet-name)
(export gnc:html-style-sheet-set-name!)
(export gnc:html-style-sheet-type)
(export gnc:html-style-sheet-set-type!)
(export gnc:html-style-sheet-options)
(export gnc:html-style-sheet-set-options!)
(export gnc:html-style-sheet-renderer)
(export gnc:html-style-sheet-set-renderer!)
(export gnc:make-html-style-sheet-internal)
(export gnc:html-style-sheet-style)
(export gnc:html-style-sheet-set-style!)
(export gnc:make-html-style-sheet)
(export gnc:restore-html-style-sheet)
(export gnc:html-style-sheet-apply-changes)
(export gnc:html-style-sheet-render)
(export gnc:get-html-style-sheets)
(export gnc:get-html-templates)
(export gnc:html-style-sheet-find)
(export gnc:html-style-sheet-remove)

;; html-acct-table.scm

(export <html-acct-table>)
(export gnc:html-acct-table?)
(export gnc:_make-html-acct-table_)
(export gnc:make-html-acct-table)
(export gnc:make-html-acct-table/env)
(export gnc:make-html-acct-table/env/accts)
(export gnc:_html-acct-table-matrix_)
(export gnc:_html-acct-table-set-matrix!_)
(export gnc:_html-acct-table-env_)
(export gnc:_html-acct-table-set-env!_)
(export gnc:html-acct-table-add-accounts!)
(export gnc:html-acct-table-num-rows)
(export gnc:html-acct-table-get-row)
(export gnc:html-acct-table-get-cell)
(export gnc:html-acct-table-set-cell!)
(export gnc:html-acct-table-get-row-env)
(export gnc:html-acct-table-set-row-env!)
(export gnc:account-code-less-p)
(export gnc:account-name-less-p)
(export gnc:account-path-less-p)
(export gnc:html-table-add-labeled-amount-line!)
(export gnc:html-table-add-account-balances)
(export gnc-commodity-table)
(export gnc:uniform-commodity?)

;; html-chart.scm

(export gnc:html-chart?)
(export gnc:make-html-chart)
(export gnc:html-chart-data)
(export gnc:html-chart-set-data!)
(export gnc:html-chart-width)
(export gnc:html-chart-set-width!)
(export gnc:html-chart-height)
(export gnc:html-chart-set-height!)
(export gnc:html-chart-type)
(export gnc:html-chart-set-type!)
(export gnc:html-chart-title)
(export gnc:html-chart-get)
(export gnc:html-chart-set!)
(export gnc:html-chart-currency-iso)
(export gnc:html-chart-set-currency-iso!)
(export gnc:html-chart-currency-symbol)
(export gnc:html-chart-set-currency-symbol!)
(export gnc:html-chart-render)

;; html-table.scm

(export <html-table>)
(export gnc:html-table?)
(export <html-table-cell>)
(export gnc:make-html-table-cell-internal)
(export gnc:make-html-table-cell)
(export gnc:make-html-table-cell/size)
(export gnc:make-html-table-cell/markup)
(export gnc:make-html-table-cell/size/markup)
(export gnc:make-html-table-header-cell)
(export gnc:make-html-table-header-cell/markup)
(export gnc:make-html-table-header-cell/size)
(export gnc:make-html-table-cell/min-width)
(export gnc:html-table-cell?)
(export gnc:html-table-cell-rowspan)
(export gnc:html-table-cell-set-rowspan!)
(export gnc:html-table-cell-colspan)
(export gnc:html-table-cell-set-colspan!)
(export gnc:html-table-cell-tag)
(export gnc:html-table-cell-set-tag!)
(export gnc:html-table-cell-data)
(export gnc:html-table-cell-set-data-internal!)
(export gnc:html-table-cell-style)
(export gnc:html-table-cell-set-style-internal!)
(export gnc:html-table-cell-set-style!)
(export gnc:html-table-cell-append-objects!)
(export gnc:html-table-cell-render)
(export gnc:make-html-table-internal)
(export gnc:make-html-table)
(export gnc:html-table-data)
(export gnc:html-table-set-data!)
(export gnc:html-table-caption)
(export gnc:html-table-set-caption!)
(export gnc:html-table-col-headers)
(export gnc:html-table-set-col-headers!)
(export gnc:html-table-row-headers)
(export gnc:html-table-set-row-headers!)
(export gnc:html-table-style)
(export gnc:html-table-set-style-internal!)
(export gnc:html-table-row-styles)
(export gnc:html-table-set-row-styles!)
(export gnc:html-table-row-markup-table)
(export gnc:html-table-row-markup)
(export gnc:html-table-set-row-markup-table!)
(export gnc:html-table-set-row-markup!)
(export gnc:html-table-col-styles)
(export gnc:html-table-set-col-styles!)
(export gnc:html-table-col-headers-style)
(export gnc:html-table-set-col-headers-style!)
(export gnc:html-table-row-headers-style)
(export gnc:html-table-set-row-headers-style!)
(export gnc:html-table-set-last-row-style!)
(export gnc:html-table-set-style!)
(export gnc:html-table-set-col-style!)
(export gnc:html-table-set-row-style!)
(export gnc:html-table-row-style)
(export gnc:html-table-col-style)
(export gnc:html-table-num-rows)
(export gnc:html-table-set-num-rows-internal!)
(export gnc:html-table-num-columns)
(export gnc:html-table-append-row/markup!)
(export gnc:html-table-prepend-row/markup!)
(export gnc:html-table-append-row!)
(export gnc:html-table-prepend-row!)
(export gnc:html-table-get-cell)
(export gnc:html-table-set-cell!)
(export gnc:html-table-set-cell/tag!)
(export gnc:html-table-append-column!)
(export gnc:html-table-render)

;; html-anytag.scm
(export <html-anytag>)
(export html-anytag?)
(export gnc:html-anytag-data)
(export gnc:html-anytag-set-data!)
(export gnc:html-anytag-style)
(export gnc:html-anytag-append-data!)
(export gnc:html-anytag-set-style!)
(export gnc:html-anytag-render div doc)
(export gnc:make-html-div)
(export gnc:make-html-div/markup)
(export gnc:make-html-span)
(export gnc:make-html-span/markup)

;; html-text.scm

(export <html-text>)
(export gnc:html-text?)
(export gnc:make-html-text-internal)
(export gnc:make-html-text)
(export gnc:html-text?)
(export gnc:html-text-body)
(export gnc:html-text-set-body-internal!)
(export gnc:html-text-set-body!)
(export gnc:html-text-style)
(export gnc:html-text-set-style-internal!)
(export gnc:html-text-set-style!)
(export gnc:html-text-append!)
(export gnc:html-markup)
(export gnc:html-markup/attr)
(export gnc:html-markup/no-end)
(export gnc:html-markup/attr/no-end)
(export gnc:html-markup/format)
(export gnc:html-markup-p)
(export gnc:html-markup-tt)
(export gnc:html-markup-em)
(export gnc:html-markup-b)
(export gnc:html-markup-i)
(export gnc:html-markup-h1)
(export gnc:html-markup-h2)
(export gnc:html-markup-h3)
(export gnc:html-markup-br)
(export gnc:html-markup-hr)
(export gnc:html-markup-ul)
(export gnc:html-markup-anchor)
(export gnc:html-markup-img)
(export gnc:html-text-render)
(export gnc:html-text-render-markup)

;; report-utilities.scm

(export list-ref-safe)
(export list-set-safe!)
(export gnc:monetary->string)
(export gnc:account-has-shares?)
(export gnc:account-is-stock?)
(export gnc:account-is-inc-exp?)
(export gnc:filter-accountlist-type)
(export gnc:decompose-accountlist)
(export gnc:account-get-type-string-plural)
(export gnc:accounts-get-commodities)
(export gnc:get-current-account-tree-depth)
(export gnc:accounts-and-all-descendants)
(export gnc:make-value-collector)
(export gnc:make-commodity-collector)
(export gnc:collector+)
(export gnc:collector-)
(export gnc:commodity-collector-get-negated)
(export gnc:account-accumulate-at-dates)
(export gnc:account-get-balance-at-date)
(export gnc:account-get-balances-at-dates)
(export gnc:account-get-comm-balance-at-date)
(export gnc:account-get-comm-value-interval)
(export gnc:account-get-comm-value-at-date)
(export gnc:accounts-get-balance-helper)
(export gnc:accounts-get-comm-total-profit)
(export gnc:accounts-get-comm-total-income)
(export gnc:accounts-get-comm-total-expense)
(export gnc:accounts-get-comm-total-assets)
(export gnc:account-get-balance-interval)
(export gnc:account-get-comm-balance-interval)
(export gnc:accountlist-get-comm-balance-interval)
(export gnc:accountlist-get-comm-balance-interval-with-closing)
(export gnc:accountlist-get-comm-balance-at-date)
(export gnc:accountlist-get-comm-balance-at-date-with-closing)
(export gnc:query-set-match-non-voids-only!)
(export gnc:query-set-match-voids-only!)
(export gnc:split-voided?)
(export gnc:report-starting)
(export gnc:report-render-starting)
(export gnc:report-percent-done)
(export gnc:report-finished)
(export gnc:accounts-count-splits)
(export gnc-commodity-collector-allzero?)
(export gnc:monetary+)
(export gnc:monetaries-add)
(export gnc:account-get-trans-type-balance-interval)
(export gnc:account-get-trans-type-balance-interval-with-closing)
(export gnc:account-get-trans-type-splits-interval)
(export gnc:budget-get-start-date)
(export gnc:budget-get-end-date)
(export gnc:budget-account-get-net)
(export gnc:budget-accountlist-get-net)
(export gnc:budget-account-get-initial-balance)
(export gnc:budget-accountlist-get-initial-balance)
(export budget-account-sum budget)
(export gnc:get-account-period-rolledup-budget-value)
(export gnc:budget-account-get-rolledup-net)
(export gnc:get-assoc-account-balances)
(export gnc:select-assoc-account-balance)
(export gnc:get-assoc-account-balances-total)
(export make-file-url)
(export gnc:strify)
(export gnc:pk)
(export gnc:dump-book)
(export gnc:dump-invoices)

;; trep-engine.scm
(export gnc:trep-options-generator)
(export gnc:trep-renderer)

;; report-register-hooks.scm

(export gnc:register-report-hook)
(export gnc:lookup-register-report)


(load-from-path "gnucash/report/commodity-utilities")
(load-from-path "gnucash/report/html-chart")
(load-from-path "gnucash/report/html-barchart")
(load-from-path "gnucash/report/html-document")
(load-from-path "gnucash/report/html-piechart")
(load-from-path "gnucash/report/html-scatter")
(load-from-path "gnucash/report/html-linechart")
(load-from-path "gnucash/report/html-style-info")
(load-from-path "gnucash/report/html-fonts")

(load-from-path "gnucash/report/html-style-sheet")
(load-from-path "gnucash/report/html-anytag")
(load-from-path "gnucash/report/html-table")
(load-from-path "gnucash/report/html-text")
(load-from-path "gnucash/report/html-acct-table")
(load-from-path "gnucash/report/html-utilities")
(load-from-path "gnucash/report/options-utilities")
(load-from-path "gnucash/report/report-utilities")
(load-from-path "gnucash/report/report-register-hooks")
(load-from-path "gnucash/report/report-core")
(load-from-path "gnucash/report/trep-engine")

;; Report uuids used for the category barcharts

(export category-barchart-income-uuid
        category-barchart-expense-uuid
        category-barchart-asset-uuid
        category-barchart-liability-uuid)

(define category-barchart-income-uuid "44f81bee049b4b3ea908f8dac9a9474e")
(define category-barchart-expense-uuid "b1f15b2052c149df93e698fe85a81ea6")
(define category-barchart-asset-uuid "e9cf815f79db44bcb637d0295093ae3d")
(define category-barchart-liability-uuid "faf410e8f8da481fbc09e4763da40bcc")

(export report-module-loader)
;; Given a list of module prefixes, load all guile modules with these prefixes
;; This assumes the modules are located on the file system in a
;; path matching the module prefix
;; For example passing
;; '('(gnucash report stylesheets) '(gnucash reports standard))
;; will search for scm files in
;; - <gnc-guile-dir>/gnucash/report/stylesheets
;; - <gnc-guile-dir>/gnucash/reports/standard
;; and try to load them.
;; This function is non-recursive so it won't
;; descend in subdirectories.
(define (report-module-loader mod-prefix-list)

  ;; Returns a list of files in a directory
  ;;
  ;; Param:
  ;;   dir - directory name
  ;;
  ;; Return value:
  ;;   list of files in the directory
  (define (directory-files dir)
    (cond
      ((file-exists? dir)
       (let ((dir-stream (opendir dir)))
            (let loop ((fname (readdir dir-stream))
                       (acc '()))
                      (cond
                        ((eof-object? fname)
                         (closedir dir-stream)
                         acc)
                        (else
                          (loop (readdir dir-stream)
                                (if (string-suffix? ".scm" fname)
                                    (cons (string-drop-right fname 4) acc)
                                    acc)))))))
      (else
        (gnc:warn "Can't access " dir ".\nEmpty list will be returned.")
        '())))

    ;; Return a list of symbols representing modules in the directory
    ;; matching the prefix
    ;;
    ;; Return value:
    ;;  List of symbols for modules
  (define (get-module-list mod-prefix)
    (let* ((subdir (string-join (map symbol->string mod-prefix) "/"))
           (mod-dir (gnc-build-scm-path subdir))
           (mod-list (directory-files mod-dir)))
          (gnc:debug "rpt-subdir=" subdir)
          (gnc:debug "mod-dir=" mod-dir)
          (gnc:debug "dir-files=" mod-list)
     (map string->symbol mod-list)))

  (for-each
    (lambda (mod-prefix)
      (for-each
        (lambda (mod-file)
                (let* ((module (append mod-prefix (list mod-file))))
                      (module-use!
                       (current-module)
                       (resolve-interface module))))
        (get-module-list mod-prefix)))
    mod-prefix-list))

;; Add hooks when this module is loaded
(gnc-hook-add-scm-dangler HOOK-SAVE-OPTIONS gnc:save-style-sheet-options)
