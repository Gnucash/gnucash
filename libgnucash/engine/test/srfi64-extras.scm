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

(define-module (gnucash engine test srfi64-extras))
(use-modules (srfi srfi-64))

(export gnc:test-runner)
(define (gnc:test-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] line:~a, test: ~a\n"
                (test-result-ref runner 'result-kind)
                (test-result-ref runner 'source-line)
                (test-runner-test-name runner))
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (1+ num-passed)))
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a\n -> expected: ~s\n -> obtained: ~s\n"
                       (string-join (test-runner-group-path runner) "/")
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value)))
           (set! num-failed (1+ num-failed)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source:~a\npass = ~a, fail = ~a\n"
                (test-result-ref runner 'source-file) num-passed num-failed)
        (zero? num-failed)))
    runner))
