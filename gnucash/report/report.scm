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

(load-and-reexport (gnucash html)
                   (gnucash report html-style-sheet)
                   (gnucash report report-register-hooks)
                   (gnucash report html-utilities)
                   (gnucash report commodity-utilities)
                   (gnucash report report-utilities)
                   (gnucash report options-utilities)
                   (gnucash report html-chart)
                   (gnucash report html-fonts)
                   (gnucash report html-text)
                   (gnucash report html-anytag)
                   (gnucash report html-document)
                   (gnucash report html-table)
                   (gnucash report html-acct-table)
                   (gnucash report html-style-info)
                   (gnucash report trep-engine)
                   (gnucash report report-core))

;; Report uuids used for the category barcharts

(export report-module-loader)
(export category-barchart-income-uuid)
(export category-barchart-expense-uuid)
(export category-barchart-asset-uuid)
(export category-barchart-liability-uuid)

(define category-barchart-income-uuid "44f81bee049b4b3ea908f8dac9a9474e")
(define category-barchart-expense-uuid "b1f15b2052c149df93e698fe85a81ea6")
(define category-barchart-asset-uuid "e9cf815f79db44bcb637d0295093ae3d")
(define category-barchart-liability-uuid "faf410e8f8da481fbc09e4763da40bcc")

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
