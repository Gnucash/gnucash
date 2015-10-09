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

(use-modules (gnucash core-utils))

(define (gnc:html-js-include file)
    (string-append
        "<script language=\"javascript\" type=\"text/javascript\" src=\"file:///"
        (gnc-path-find-localized-html-file file)
        "\"></script>\n"
    ))

(define (gnc:html-css-include file)
    (string-append
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///"
        (gnc-path-find-localized-html-file file)
        "\" />\n"
    ))

(define (jqplot-escape-string s1)
    ;; Escape single and double quotes and backslashes
    (set! s1 (regexp-substitute/global #f "\\\\" s1 'pre "\\\\" 'post))
    (set! s1 (regexp-substitute/global #f "'" s1 'pre "\\'" 'post))
    (set! s1 (regexp-substitute/global #f "\"" s1 'pre "\\\"" 'post))
    ;; Escape HTML special characters
    (set! s1 (regexp-substitute/global #f "&" s1 'pre "&amp;" 'post))
    (set! s1 (regexp-substitute/global #f "<" s1 'pre "&lt;" 'post))
    (regexp-substitute/global #f ">" s1 'pre "&gt;" 'post))
