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

(define-module (test-blocks lists)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks lists")

(test-assert "parse-blocks, list changing bullet list delimiter starts a new list"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "- bar\n"
                                    "+ baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data2
                                                      ('text text-data2 "bar")))
                                   ('item item-data3
                                          ('paragraph para-data3
                                                      ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(define (list-start data)
  (assq-ref data 'start))

(test-assert "parse-blocks, list changing ordered list delimiter starts a new list"
             (match (call-with-input-string
                     (string-append "1. foo\n"
                                    "2. bar\n"
                                    "3) baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data2
                                                      ('text text-data2 "bar")))
                                   ('item item-data3
                                          ('paragraph para-data3
                                                      ('text text-data3 "foo")))))
                (eq? (list-start list-data1) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list can interrupt a paragraph"
             (match (call-with-input-string
                     (string-append "Foo\n"
                                    "- bar\n"
                                    "- baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "bar"))))
                            ('paragraph para-data3
                                        ('text text-data3 "Foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list there can be blank lines between items, but two blank lines end a list"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "- bar\n"
                                    "\n"
                                    "\n"
                                    "- baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                           ('list list-data2
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "bar")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines between blocks within a list item also ends a list"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "\n"
                                    "  bar\n"
                                    "- baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                            ('paragraph para-data2
                                        ('text text-data2 "bar"))
                           ('list list-data2
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines end all containing lists"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "  - bar\n"
                                    "    - baz\n"
                                    "\n"
                                    "\n"
                                    "      bim")
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data "  bim")
                           ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('list list-data3
                                                              ('item item-data3
                                                                     ('paragraph para-data1
                                                                                 ('text text-data1 "baz"))))
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "bar"))))
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines can separate consecutive lists of the same type"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "- bar\n"
                                    "\n"
                                    "\n"
                                    "- baz\n"
                                    "- bim")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bim")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "baz"))))
                           ('list list-data2
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "bar")))                               
                                  ('item item-data4
                                         ('paragraph para-data4
                                                     ('text text-data4 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines can separate an indented code block that would be a paragraph"
             (match (call-with-input-string
                     (string-append "-   foo\n"
                                    "\n"
                                    "    notcode\n"
                                    "\n"
                                    "-   foo\n"
                                    "\n"
                                    "\n"
                                    "    code")
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data "code")
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "foo")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "notcode"))
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-expect-fail 2)
(test-assert "parse-blocks, list need not be indented to the same level"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    " - b\n"
                                    "  - c\n"
                                    "   - d\n"
                                    "    - e\n"
                                    "   - f\n"
                                    "  - g\n"
                                    " - h\n"
                                    "- i")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "i")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "h")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "g")))
                                  ('item item-data4
                                         ('paragraph para-data4
                                                     ('text text-data4 "f")))
                                  ('item item-data5
                                         ('paragraph para-data5
                                                     ('text text-data5 "e")))
                                  ('item item-data6
                                         ('paragraph para-data6
                                                     ('text text-data6 "d")))
                                  ('item item-data7
                                         ('paragraph para-data7
                                                     ('text text-data7 "c")))
                                  ('item item-data8
                                         ('paragraph para-data8
                                                     ('text text-data8 "b")))
                                  ('item item-data9
                                         ('paragraph para-data9
                                                     ('text text-data9 "a")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list need not be indented to the same level"
             (match (call-with-input-string
                     (string-append "1. a\n"
                                    "\n"
                                    "  2. b\n"
                                    "\n"
                                    "    3. c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "b")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "a")))))
                #t)
               (x (pk 'fail x #f))))

(define (list-tight? data)
  (assq-ref data 'tight))

(test-expect-fail 4)
(test-assert "parse-blocks, list with a blank line between items is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- b\n"
                                    "\n"
                                    "- c\n")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "b")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, list with a blank line between items is loose"
             (match (call-with-input-string
                     (string-append "* a\n"
                                    "*\n"
                                    "\n"
                                    "* c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list with a list item with two block level elements is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- b\n"
                                    "\n"
                                    "  c\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "c"))
                                         ('paragraph para-data3
                                                     ('text text-data3 "b")))
                                  ('item item-data3
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list with a list item with two block level elements is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- b\n"
                                    "\n"
                                    "  [ref]: /url\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 ""))
                                         ('paragraph para-data3
                                                     ('text text-data3 "b")))
                                  ('item item-data3
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list with blank lines in code block are tight"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- ```\n"
                                    "  b\n"
                                    "\n"
                                    "\n"
                                    "  ```\n"
                                    "- c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2
                                         ('fenced-code code-data "b\n\n"))
                                  ('item item-data3
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data))
               (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-blocks, list is tight even if a sublist is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  - b\n"
                                    "\n"
                                    "    c\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data3
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "c"))
                                                       ('paragraph para-data3
                                                                   ('text text-data3 "b"))))
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list is tight if blank line is inside a block quote"
             (match (call-with-input-string
                     (string-append "* a\n"
                                    "  > b\n"
                                    "  >\n"
                                    "* c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data3
                                         ('block-quote quote-data
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "b")))
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list is tight if the consecutive block elements are not separated by blank lines"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  > b\n"
                                    "  ```\n"
                                    "  c\n"
                                    "  ```\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data3
                                         ('fenced-code code-data "c")
                                         ('block-quote quote-data
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "b")))
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list a single paragraph is tight"
             (match (call-with-input-string
                     "- a"
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list a single paragraph is tight"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  - b")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                                   ('text text-data1 "b"))))
                                         ('paragraph para-data1
                                                     ('text text-data1 "a")))))
                (and (list-tight? list-data1)
                     (list-tight? list-data2)))
               (x (pk 'fail x #f))))

(test-expect-fail 3)
(test-assert "parse-blocks, list is loose if blank line between two block elements"
             (match (call-with-input-string
                     (string-append "1. ```\n"
                                    "   foo\n"
                                    "   ```\n"
                                    "\n"
                                    "   bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('fenced-code code-data "foo"))))
                (not (list-tight? list-data1)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, outer list is loose and inner list tight"
             (match (call-with-input-string
                     (string-append "* foo\n"
                                    "  * bar\n"
                                    "\n"
                                    "  baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "bar"))))
                                         ('paragraph para-data1
                                                     ('text text-data1 "foo"))
                                        )))
                (and (not (list-tight? list-data1))
                     (list-tight? list-data2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, outer list is loose and inner list tight"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  - b\n"
                                    "  - c\n"
                                    "\n"
                                    "- d\n"
                                    "  - e\n"
                                    "  - f")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "f")))
                                                ('item item-data3
                                                       ('paragraph para-data3
                                                                   ('text text-data3 "e"))))
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data4
                                         ('list list-data3
                                                ('item item-data4
                                                       ('paragraph para-data4
                                                                   ('text text-data4 "c")))
                                                ('item item-data5
                                                       ('paragraph para-data5
                                                                   ('text text-data5 "b"))))
                                         ('paragraph para-data6
                                                     ('text text-data6 "a")))))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
