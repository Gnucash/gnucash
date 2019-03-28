;;; (tests test-parser) --- Guile JSON implementation.

;; Copyright (C) 2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-json. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Unit tests the JSON parser

;;; Code:

(define-module (tests test-parser)
  #:use-module (srfi srfi-64)
  #:use-module (json)
  #:use-module (tests runner))

(test-runner-factory json:test-runner)

(test-begin "test-parser")

;; Numbers
(test-equal 1234 (json-string->scm "1234"))
(test-equal -1234 (json-string->scm "-1234"))
(test-equal -54.897 (json-string->scm "-54.897"))
(test-equal 1000.0 (json-string->scm "1e3"))
(test-equal 0.001 (json-string->scm "1E-3"))

;; Strings
(test-equal "hello guile!" (json-string->scm "\"hello guile!\""))
(test-equal "你好 guile!" (json-string->scm "\"你好 guile!\""))
(test-equal "你好 guile!" (json-string->scm "\"\\u4f60\\u597d guile!\""))

;; Boolean
(test-equal #t (json-string->scm "true"))
(test-equal #f (json-string->scm "false"))

;; Null
(test-equal #nil (json-string->scm "null"))

;; Arrays
(test-equal #() (json-string->scm "[]"))
(test-equal #(1 2 3 4) (json-string->scm "[1,2,3,4]"))
(test-equal #(1 2 #(3 4) #(5 6 #(7 8))) (json-string->scm "[1,2,[3,4],[5,6,[7,8]]]" ))
(test-equal #(1 "two" 3 "four") (json-string->scm "[1,\"two\",3,\"four\"]"))

;; Objects
(test-equal '(("foo" . "bar")) (json-string->scm "{\"foo\":\"bar\"}"))
(test-equal '(("foo" . "bar")) (json-string->scm "{\"foo\":\"bar\"}"))
(test-equal '(("foo" . #(1 2 3))) (json-string->scm "{\"foo\":[1,2,3]}"))
(test-equal '(("foo" . (("bar" . #(1 2 3))))) (json-string->scm "{\"foo\":{\"bar\":[1,2,3]}}"))
(test-equal '(("foo" . #(1 (("two" . "three"))))) (json-string->scm "{\"foo\":[1,{\"two\":\"three\"}]}"))

;; Since the following JSON object contains more than one key-value pair, we
;; can't use "test-equal" directly since the output could be unordered.
(define book (json-string->scm "{\"title\":\"A book\",\"author\":\"An author\",\"price\":29.99}"))
(test-equal "A book" (assoc-ref book "title"))
(test-equal "An author" (assoc-ref book "author"))
(test-equal 29.99 (assoc-ref book "price"))

(exit (if (test-end "test-parser") 0 1))

;;; (tests test-parser) ends here
