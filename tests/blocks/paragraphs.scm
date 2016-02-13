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

(define-module (test-blocks paragraphs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks paragraphs")

(test-equal "parse-blocks, empty document"
            (call-with-input-string "" parse-blocks)
            '(document ((closed . #f))))

(test-equal "parse-blocks, simple paragraph"
            (call-with-input-string "foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-assert "parse-blocks, paragraph leading spaces skipped"
             (match (call-with-input-string "  aaa\n bbb" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "aaa\nbbb")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, paragraph lines after first may ident any ammount"
             (match (call-with-input-string "aaa\n          bbb\n                    ccc" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "aaa\nbbb\nccc")))
                #t)
               (x (pk 'fail x #f))))

(test-equal "parse-blocks, multiline paragraph"
            (call-with-input-string "foo\nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo\nbar"))))

(test-assert "parse-blocks, code block does not interrupt paragraph"
             (match (call-with-input-string "foo\n    bar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "foo\nbar")))
                #t)
               (x (pk 'fail x #f))))

(test-equal "parse-blocks, multiline paragraph preserves line ending spaces"
            (call-with-input-string "foo   \nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo   \nbar"))))

(test-assert "parse-blocks, paragraph multiple blank lines have no affect"
            (match (call-with-input-string "aaa\n\nbbb" parse-blocks)
              (('document doc-data
                          ('paragraph para-data1
                                      ('text text-data1 "bbb"))
                          ('paragraph para-data2
                                      ('text text-data2 "aaa")))
               #t)
              (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
