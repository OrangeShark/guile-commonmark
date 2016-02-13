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

(define-module (test-blocks code-blocks)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks code-blocks")

(test-assert "parse-blocks, indented code block"
             (match (call-with-input-string "    a simple\n      indented code block" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "a simple\n  indented code block"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block list takes precedence in list item"
             (match (call-with-input-string "1.  foo\n\n    bar" parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data 
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block chunks separated by blank lines"
             (match (call-with-input-string "    chunk1\n\n    chunk2\n  \n \n \n    chunk3" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "chunk1\n\nchunk2\n\n\n\nchunk3"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block includes spaces"
             (match (call-with-input-string "    chunk1\n      \n      chunk2" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "chunk1\n  \n  chunk2"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block fewer than four leading spaces end block"
             (match (call-with-input-string "    foo\nbar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "bar"))
                           ('code-block code-data
                                        "foo"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block and occur before or after blocks that aren't paragraphs"
             (match (call-with-input-string
                     "# Heading\n    foo\nHeading\n------\n    foo\n----" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('code-block code-data "foo")
                           ('heading heading-data1
                                     ('text text-data1 "Heading"))
                           ('code-block code-data "foo")
                           ('heading heading-data2
                                     ('text text-data2 "Heading")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block first line can have more than four spaces"
             (match (call-with-input-string "        foo\n    bar" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "    foo\nbar"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code blank lines preceding or following are not included"
             (match (call-with-input-string "\n    \n    foo\n    \n\n" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "foo"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code trailing spaces are included"
             (match (call-with-input-string "    foo  " parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "foo  "))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
