;; startup.scm
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;; Load all the files we need from wherever the user has specified.
;; None of these loads will be affected by any command line arguments
;; since arguments aren't parsed until gnc:main is executed.

(if gnc:*load-slib-backup*
    (gnc:load "slib-backup.scm"))

(gnc:load "depend.scm")
(gnc:load "config-var.scm")
(gnc:load "utilities.scm")
(gnc:load "path.scm")
(gnc:load "c-interface.scm")
(gnc:load "commodity-table.scm")
(gnc:load "engine-init.scm")
(gnc:load "engine-interface.scm")
(gnc:load "options.scm")
(gnc:load "prefs.scm")
(gnc:load "command-line.scm")
(gnc:load "hooks.scm")
(gnc:load "tip-of-the-day.scm")
(gnc:load "main.scm")
