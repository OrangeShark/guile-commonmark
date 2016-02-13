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

(define-module (test-blocks atx-headings)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks atx-headings")

(define (heading-level heading-data)
  (assq-ref heading-data 'level))

(test-assert "parse-blocks, atx headings"
             (match (call-with-input-string
                     "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo" parse-blocks)
               (('document doc-data
                           ('heading heading-data6
                                    ('text text-data6 "foo"))
                           ('heading heading-data5
                                    ('text text-data5 "foo"))
                           ('heading heading-data4
                                    ('text text-data4 "foo"))
                           ('heading heading-data3
                                    ('text text-data3 "foo"))
                           ('heading heading-data2
                                    ('text text-data2 "foo"))
                           ('heading heading-data1
                                    ('text text-data1 "foo")))
                (and (eq? (heading-level heading-data6) 6)
                     (eq? (heading-level heading-data5) 5)
                     (eq? (heading-level heading-data4) 4)
                     (eq? (heading-level heading-data3) 3)
                     (eq? (heading-level heading-data2) 2)
                     (eq? (heading-level heading-data1) 1)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings not more than 6 #"
             (match (call-with-input-string "####### foo" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                      ('text text-data "####### foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings requires an empty space"
             (match (call-with-input-string "#hashtag" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "#hashtag")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings not a heading when # is escaped"
             (match (call-with-input-string "\\## foo" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "\\## foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings leading and trailing blanks are ignored"
             (match (call-with-input-string "#                  foo                  " parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                       ('text text-data "foo")))
                (eq? (heading-level heading-data) 1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings closing # characters are optional"
             (match (call-with-input-string "## foo ##\n  ###   bar    ###" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 "bar"))
                           ('heading heading-data2
                                     ('text text-data2 "foo")))
                (and (eq? (heading-level heading-data1) 3)
                     (eq? (heading-level heading-data2) 2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings spaces are allowed after closing sequence"
             (match (call-with-input-string "### foo ###     " parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                     ('text text-data "foo")))
                (eq? (heading-level heading-data) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings nonspace character after closing sequence"
             (match (call-with-input-string "### foo ### b" parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                     ('text text-data "foo ### b")))
                (eq? (heading-level heading-data) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings closing sequence must be preceded by a space"
             (match (call-with-input-string "# foo#" parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                     ('text text-data "foo#")))
                (eq? (heading-level heading-data) 1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings can be empty"
             (match (call-with-input-string "## \n#\n### ###" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 ""))
                           ('heading heading-data2
                                     ('text text-data2 ""))
                           ('heading heading-data3
                                     ('text text-data3 "")))
                (and (eq? (heading-level heading-data1) 3)
                     (eq? (heading-level heading-data2) 1)
                     (eq? (heading-level heading-data3) 2)))
               (x (pk 'fail x #f))))


(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
