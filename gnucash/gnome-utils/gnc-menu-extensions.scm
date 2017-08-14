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

(define (gnc:make-extension
	 ;; The type of extension item, either 'menu, 'menu-item, or 'separator
         type
         ;; The name of the extension in the menu
         name
         ;; The guid of object the menu will refer to
         guid
         ;; The tooltip
         documentation-string
         ;; A list of names indicating the menus under which this item is
         ;; located. The last item indicates the item *after* which this
         ;; extension will go.
         path
         ;; The script to call when the menu item is selected
         script)
  (vector type
          name
          guid
          documentation-string
          path
          script))

(define (gnc:extension-type extension)
  (vector-ref extension 0))
(define (gnc:extension-name extension)
  (vector-ref extension 1))
(define (gnc:extension-guid extension)
  (vector-ref extension 2))
(define (gnc:extension-documentation extension)
  (vector-ref extension 3))
(define (gnc:extension-path extension)
  (vector-ref extension 4))
(define (gnc:extension-script extension)
  (vector-ref extension 5))

(define (gnc:make-menu-item name guid documentation-string path script)
  (gnc:make-extension 'menu-item name guid documentation-string path script))

(define (gnc:make-menu name path)
  (gnc:make-extension 'menu name name "" path #f))

(define (gnc:make-separator path)
  (gnc:make-extension 'separator "" "" "" path #f))
