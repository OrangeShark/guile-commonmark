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

(define-module (test-blocks setext-headings)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks setext-headings")

(define (heading-level heading-data)
  (assq-ref heading-data 'level))

(test-assert "parse-blocks, setext headings"
             (match (call-with-input-string "Foo\n-------------------------\n\nBar\n=" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 "Bar"))
                           ('heading heading-data2
                                     ('text text-data2 "Foo")))
                (and (eq? (heading-level heading-data1) 1)
                     (eq? (heading-level heading-data2) 2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext headings up to three space indents"
             (match (call-with-input-string
                     "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 "Foo"))
                           ('heading heading-data2
                                     ('text text-data2 "Foo"))
                           ('heading heading-data3
                                     ('text text-data3 "Foo")))
                (and (eq? (heading-level heading-data1) 1)
                     (eq? (heading-level heading-data2) 2)
                     (eq? (heading-level heading-data3) 2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext headings four space indents too much"
             (match (call-with-input-string
                     "    Foo\n    ---\n\n    Foo\n---" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('code-block code-data "Foo\n---\n\nFoo"))
                #t)
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, setext headings four space indent underline too much"
             (match (call-with-input-string "Foo\n    ---" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "Foo\n---")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext headings underline cannot contain interal spaces"
             (match (call-with-input-string "Foo\n= =\n\nFoo\n--- -" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('paragraph para-data1
                                       ('text text-data1 "Foo"))
                           ('paragraph para-data2
                                       ('text text-data2 "Foo\n= =")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext heading cannot interrupt a paragraph"
             (match (call-with-input-string "Foo\nBar\n---" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('paragraph para-data
                                       ('text text-data "Foo\nBar")))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
