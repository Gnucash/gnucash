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

(define-module (gnucash gnome-utils gnc-menu-extensions))

(use-modules (srfi srfi-9))

(export gnc:extension-documentation)
(export gnc:extension-guid)
(export gnc:extension-name)
(export gnc:extension-path)
(export gnc:extension-script)
(export gnc:extension-type)
(export gnc:make-extension)
(export gnc:make-menu)
(export gnc:make-menu-item)
(export gnc:make-separator)

(define-record-type :extension
  (gnc:make-extension type name guid documentation-string path script)
  :extension?
  (type gnc:extension-type)
  (name gnc:extension-name)
  (guid gnc:extension-guid)
  (documentation-string gnc:extension-documentation)
  (path gnc:extension-path)
  (script gnc:extension-script))

(define (gnc:make-menu-item name guid documentation-string path script)
  (gnc:make-extension 'menu-item name guid documentation-string path script))

(define (gnc:make-menu name path)
  (gnc:make-extension 'menu name name "" path #f))

(define (gnc:make-separator path)
  (gnc:make-extension 'separator "" "" "" path #f))
