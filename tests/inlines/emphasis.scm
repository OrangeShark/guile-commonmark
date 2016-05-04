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

(define-module (test-inlines emphasis)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(test-begin "inlines emphasis")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(define (em? node-data)
  (eq? 'em (assq-ref node-data 'type)))

(test-assert "parse-inlines, simple emphasis"
  (match (parse-inlines (make-paragraph "*foo bar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "foo bar"))))
     (em? emphasis-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, not emphasis because whitespace after *"
  (match (parse-inlines (make-paragraph "a * foo bar*"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data1 "*")
                            ('text text-data2 " foo bar")
                            ('text text-data3 "*")
                            ('text text-data4 "a ")))
     #t)
    (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
