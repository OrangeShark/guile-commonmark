;; Copyright (C) 2016  Erik Edrosa <erik.edrosa@gmail.com>
;;
;; This file is part of guile-commonmark
;;
;; guile-commonmark is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-commonmark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-commonmark.  If not, see <http://www.gnu.org/licenses/>.

(define-module (test-blocks)
  #:use-module (srfi srfi-64)
  #:use-module (commonmark blocks))

(test-begin "blocks")

(test-equal "parse-blocks, empty document"
            (call-with-input-string "" parse-blocks)
            '(document ((closed . #f))))

(test-equal "parse-blocks, simple paragraph"
            (call-with-input-string "foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-equal "parse-blocks, three space simple paragraph"
            (call-with-input-string "   foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-equal "parse-blocks, multiline paragraph"
            (call-with-input-string "foo
bar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo
bar"))))

;; not fixed yet
(test-expect-fail "parse-blocks, code block does not interrupt paragraph")
(test-equal "parse-blocks, code block does not interrupt paragraph"
            (call-with-input-string "foo
     bar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo
bar"))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
