;; Copyright (C) 2016, 2018  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (test-inlines softbreak)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))


(test-begin "inlines softbreak")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(test-assert "parse-inlines, any ASCII punctuation character may be backslashed-escaped"
  (match (parse-inlines (make-paragraph "foo\nbaz"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "baz")
                            ('softbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, spaces at the end of the line and beginning of
the next line are removed"
  (match (parse-inlines (make-paragraph "foo \n baz"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "baz")
                            ('softbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-end)
