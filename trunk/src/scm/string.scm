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

(use-modules (srfi srfi-13))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:string-rcontains
;;
;;  Similar to string-contains, but searches from the right.
;;
;;  Example: (gnc:string-rcontains "foobarfoobarf" "bar")
;;           returns 9.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:string-rcontains s1 s2)
  (let ((s2len (string-length s2)))
    (let loop ((i (string-contains s1 s2))
               (retval #f))
      (if i
          (loop (string-contains s1 s2 (+ i s2len)) i)
          retval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-count
;;
;;  Similar to string-count, but searches for a substring rather
;;  than a single character.
;;
;;  Example: (gnc:substring-count "foobarfoobarfoo" "bar")
;;           returns 2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:substring-count s1 s2)
  (let ((s2len (string-length s2)))
    (let loop ((i (string-contains s1 s2))
               (retval 0))
      (if i
          (loop (string-contains s1 s2 (+ i s2len)) (+ 1 retval))
          retval))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-split
;;
;;  Similar to string-split, but the delimiter is a string
;;  rather than a single character.
;;
;;  Example: (gnc:substring-split "foobarfoobarf" "bar") returns
;;           ("foo" "foo" "f").
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:substring-split s1 s2)
  (let ((i (string-contains s1 s2)))
    (if i
        (cons (substring s1 0 i)
              (gnc:substring-split (substring s1 (+ i (string-length s2))) s2))
        (list s1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:substring-replace
;;
;;  Search for all occurrences in string "s1" of string "s2" and
;;  replace them with string "s3".
;;
;;  Example: (gnc:substring-replace "foobarfoobar" "bar" "xyz")
;;           returns "fooxyzfooxyz".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:substring-replace s1 s2 s3)
  (let ((s2len (string-length s2)))
    (let loop ((start1 0)
               (i (string-contains s1 s2)))
      (if i
          (string-append (substring s1 start1 i)
                         s3
                         (loop (+ i s2len) (string-contains s1 s2 (+ i s2len))))
          (substring s1 start1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:string-replace-char
;;
;;  Replaces all occurrences in string "s" of character "old"
;;  with character "new".
;;
;;  Example: (gnc:string-replace-char "foo" #\o #\c) returns
;;           "fcc".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:string-replace-char s old new)
  (string-map (lambda (c) (if (char=? c old) new c)) s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:string-delete-chars
;;
;;  Filter string "s", retaining only those characters that do not
;;  appear in string "chars".
;;
;;  Example: (gnc:string-delete-chars "abcd" "cb") returns "ad".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:string-delete-chars s chars)
  (string-delete s (lambda (c) (string-index chars c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:list-display
;;
;;  Run the display procedure on each element in a list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:list-display lst)
  (for-each (lambda (elt) (display elt)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  gnc:list-display-to-string
;;
;;  Return a string containing the output that would be generated
;;  by running the display procedure on each element in a list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (gnc:list-display-to-string lst)
  (with-output-to-string (lambda () (gnc:list-display lst))))

