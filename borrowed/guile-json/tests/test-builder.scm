;;; (tests test-builder) --- Guile JSON implementation.

;; Copyright (C) 2018, 2019 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; Unit tests the JSON builder

;;; Code:

(define-module (tests test-builder)
  #:use-module (srfi srfi-64)
  #:use-module (json)
  #:use-module (tests runner))

(test-runner-factory json:test-runner)

(test-begin "test-builder")

;; Numbers
(test-equal "1234" (scm->json-string 1234))
(test-equal "-1234" (scm->json-string -1234))
(test-equal "-54.897" (scm->json-string -54.897))
(test-equal "1000.0" (scm->json-string 1e3))
(test-equal "0.001" (scm->json-string 1e-3))
(test-equal "0.5" (scm->json-string 1/2))
(test-equal "0.75" (scm->json-string 3/4))
(test-error #t (scm->json-string 1+2i))
(test-error #t (scm->json-string +inf.0))
(test-error #t (scm->json-string -inf.0))
(test-error #t (scm->json-string +nan.0))


;; Strings
(test-equal "\"hello guile!\"" (scm->json-string "hello guile!"))
(test-equal "\"你好 guile!\"" (scm->json-string "你好 guile!"))
(test-equal "\"\\u4f60\\u597d guile!\"" (scm->json-string "你好 guile!" #:unicode #t))
(test-equal "\"</script>\"" (scm->json-string "</script>"))
(test-equal "\"<\\/script>\"" (scm->json-string "</script>" #:escape #t))

;; Boolean
(test-equal "true" (scm->json-string #t))
(test-equal "false" (scm->json-string #f))

;; Null
(test-equal "null" (scm->json-string #nil))

;; Arrays
(test-equal "[]" (scm->json-string #()))
(test-equal "[1,2,3,4]" (scm->json-string #(1 2 3 4)))
(test-equal "[1,2,[3,4],[5,6,[7,8]]]" (scm->json-string #(1 2 #(3 4) #(5 6 #(7 8)))))
(test-equal "[1,\"two\",3,\"four\"]" (scm->json-string #(1 "two" 3 "four")))

;; Objects
(test-equal "{\"foo\":\"bar\"}" (scm->json-string '((foo . bar))))
(test-equal "{\"foo\":\"bar\"}" (scm->json-string '(("foo" . "bar"))))
(test-equal "{\"foo\":[1,2,3]}" (scm->json-string '((foo . #(1 2 3)))))
(test-equal "{\"foo\":{\"bar\":[1,2,3]}}" (scm->json-string '((foo . ((bar . #(1 2 3)))))))
(test-equal "{\"foo\":[1,{\"two\":\"three\"}]}" (scm->json-string '((foo . #(1 (("two" . "three")))))))
(test-equal "{\"title\":\"A book\",\"author\":\"An author\",\"price\":29.99}"
  (scm->json-string '((title . "A book")
                      (author . "An author")
                      (price . 29.99))))

(exit (if (test-end "test-builder") 0 1))

;;; (tests test-builder) ends here
