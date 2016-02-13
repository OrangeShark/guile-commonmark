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

(define-module (test-blocks fenced-code)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks fenced-code")

(test-assert "parse-blocks, fenced code simple"
             (match (call-with-input-string "```\n<\n >\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "<\n >"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code simple with tildes"
             (match (call-with-input-string "~~~\n<\n >\n~~~" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "<\n >"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code must use the same character as opening fence"
             (match (call-with-input-string "```\naaa\n~~~\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n~~~"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code must use the same character as opening fence tildes"
             (match (call-with-input-string "````\naaa\n```\n````" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n```"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code unclosed until end of the document"
             (match (call-with-input-string "```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code closes at end of block quote"
             (match (call-with-input-string "> ```\n> aaa\n\nbbb" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "bbb"))
                           ('block-quote quote-data
                                         ('fenced-code code-data
                                                       "aaa")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can have all empty lines"
             (match (call-with-input-string "```\n\n  \n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "\n  "))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can be empty"
             (match (call-with-input-string "```\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can be indented and equivalent spaces removed"
             (match (call-with-input-string " ```\n aaa\naaa\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\naaa"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can be indented and equivalent spaces removed if present"
             (match (call-with-input-string "  ```\naaa\n  aaa\naaa\n  ```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\naaa\naaa"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code closing fence 4 spaces too much"
             (match (call-with-input-string "```\naaa\n    ```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n    ```"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code spaces are not allowed in opening fence"
             (match (call-with-input-string "``` ```\naaa" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "``` ```\naaa")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code spaces are not allowed in closing fence"
             (match (call-with-input-string "~~~~~\naaa\n~~~ ~~" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n~~~ ~~"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can interrupt paragraphs and followed by paragraphs"
             (match (call-with-input-string "foo\n```\nbar\n```\nbaz" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "baz"))
                           ('fenced-code code-data
                                         "bar")
                           ('paragraph para-data2
                                       ('text text-data2 "foo")))
                #t)
               (x (pk 'fail x #f))))

(define (info-string data)
  (assq-ref data 'info-string))

(test-assert "parse-blocks, fenced code info string"
             (match (call-with-input-string "```scheme\n(define (foo x)\n  3)\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "(define (foo x)\n  3)"))
                (equal? (info-string code-data) "scheme"))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code info string empty code"
             (match (call-with-input-string "```;\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data))
                (equal? (info-string code-data) ";"))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code coding fence cannot have info strings"
             (match (call-with-input-string "```\n``` aaa\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data "``` aaa"))
                (not (equal? (info-string code-data) "aaa")))
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
