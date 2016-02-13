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

(define-module (test-blocks list-items)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks list-items")

(test-assert "parse-blocks, list item base case"
             (match (call-with-input-string
                     "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     "- one\n\n two"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "two"))
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data2
                                                     ('text text-data2 "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     "- one\n\n  two"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "two"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     " -    one\n\n     two"
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data " two")
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     " -    one\n\n      two"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "two"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item not column"
             (match (call-with-input-string
                     "   > > 1.  one\n>>\n>>     two"
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('block-quote quote-data2
                                                       ('list list-data
                                                              ('item item-data
                                                                     ('paragraph para-data1
                                                                                 ('text text-data1 "two"))
                                                                     ('paragraph para-data2
                                                                                 ('text text-data2 "one")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item not column"
             (match (call-with-input-string
                     ">>- one\n>>\n  >  > two"
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('block-quote quote-data2
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "two"))
                                                       ('list list-data
                                                              ('item item-data
                                                                     ('paragraph para-data2
                                                                                 ('text text-data2 "one")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item needs one space after the list marker"
             (match (call-with-input-string
                     "-one\n\n2.two"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "2.two"))
                           ('paragraph para-data2
                                       ('text text-data2 "-one")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may not contain blocks with more than one blank line"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "  bar\n"
                                    "\n"
                                    "- foo\n"
                                    "\n"
                                    "\n"
                                    "  bar")
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data4
                                       ('text text-data4 "bar"))
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))
                                  ('item item-data2
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may not contain blocks with more than one blank line except fenced code block"
             (match (call-with-input-string
                     (string-append "- ```\n"
                                    "  foo\n"
                                    "\n"
                                    "\n"
                                    "  bar\n"
                                    "  ```\n"
                                    "\n"
                                    "- baz\n"
                                    "\n"
                                    "  + ```\n"
                                    "    foo\n"
                                    "\n"
                                    "\n"
                                    "    bar\n"
                                    "    ```")
                     parse-blocks)
               (('document doc-data
                           ('list list-data2
                                  ('item item-data4
                                         ('list list-data3
                                                ('item item-data5
                                                       ('fenced-code code-data2 "foo\n\n\nbar")))
                                         ('paragraph para-data5
                                                     ('text text-data5 "baz")))
                                  ('item item-data3
                                         ('fenced-code code-data1 "foo\n\n\nbar"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may contain any kind of block"
             (match (call-with-input-string
                     (string-append "1.  foo\n"
                                    "\n"
                                    "    ```\n"
                                    "    bar\n"
                                    "    ```\n"
                                    "\n"
                                    "    baz\n"
                                    "\n"
                                    "    > bam")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data3
                                                                   ('text text-data3 "bam")))
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))
                                         ('fenced-code code-data "bar")
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item contain indented code blocks that preserve empty lines"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "      bar\n"
                                    "\n"
                                    "      baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data "bar\n\nbaz")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item contain indented code blocks that preserve empty lines unless two blank lines"
             (match (call-with-input-string
                     "- foo

      bar


      baz"
                     parse-blocks)
               (('document doc-data
                           (code-block code-data1 "  baz")
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data2 "bar")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(define (list-start data)
  (assq-ref data 'start))

(test-assert "parse-blocks, list item start numbers must be nine digits or less"
             (match (call-with-input-string
                     "123456789. ok"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "ok")))))
                (equal? (list-start list-data) 123456789))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item start numbers must be nine digits or less"
             (match (call-with-input-string
                     "1234567890. not ok"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "1234567890. not ok")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may begin with 0s"
             (match (call-with-input-string
                     "0. ok"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "ok")))))
                (equal? (list-start list-data) 0))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may begin with 0s"
             (match (call-with-input-string
                     "003. ok"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "ok")))))
                (equal? (list-start list-data) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item number may not be negative"
             (match (call-with-input-string
                     "-1. not ok"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "-1. not ok")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item code block must be indented four spaces beyond the edge"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "      bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data "bar")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item code block must be indented four spaces beyond the edge"
             (match (call-with-input-string
                     (string-append "  10.  foo\n"
                                    "\n"
                                    "           bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data "bar")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item if code block is first block, contents must be indented one space after"
             (match (call-with-input-string
                     (string-append "1.     indented code\n"
                                    "\n"
                                    "   paragraph\n"
                                    "\n"
                                    "       more code")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data1 "more code")
                                         ('paragraph para-data
                                                     ('text text-data "paragraph"))
                                         ('code-block code-data2 "indented code"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item if code block is first block, additional space inside code block"
             (match (call-with-input-string
                     (string-append "1.      indented code\n"
                                    "\n"
                                    "   paragraph\n"
                                    "\n"
                                    "       more code")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data1 "more code")
                                         ('paragraph para-data
                                                     ('text text-data "paragraph"))
                                         ('code-block code-data2 " indented code"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item can start with a blank line"
             (match (call-with-input-string
                     (string-append "-\n"
                                    "  foo\n"
                                    "-\n"
                                    "  ```\n"
                                    "  bar\n"
                                    "  ```\n"
                                    "-\n"
                                    "      baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('code-block code-data1 "baz"))
                                  ('item item-data2
                                         ('fenced-code code-data2 "bar"))
                                  ('item item-data3
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item can begin with at most one blank line"
             (match (call-with-input-string
                     (string-append "-\n"
                                    "\n"
                                    "  foo")
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "foo"))
                           ('list list-data
                                  ('item item-data)))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item empty bullet list item"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "-\n"
                                    "- bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item empty bullet list item space does not matter after list marker"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "-   \n"
                                    "- bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item empty ordered list item"
             (match (call-with-input-string
                      (string-append "1. foo\n"
                                     "2.\n"
                                     "3. bar")
                      parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list may start or end with an empty list item"
             (match (call-with-input-string
                     "*"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data)))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item indented one space"
             (match (call-with-input-string
                     (string-append " 1.  A paragraph\n"
                                    "     with two lines.\n"
                                    "\n"
                                    "         indented code\n"
                                    "\n"
                                    "     > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, list item indented two spaces"
             (match (call-with-input-string
                     (string-append "  1.  A paragraph\n"
                                    "      with two lines.\n"
                                    "\n"
                                    "          indented code\n"
                                    "\n"
                                    "      > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item indented three spaces"
             (match (call-with-input-string
                     (string-append "   1.  A paragraph\n"
                                    "       with two lines.\n"
                                    "\n"
                                    "           indented code\n"
                                    "\n"
                                    "       > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item indented four spaces gives a code block"
             (match (call-with-input-string
                     (string-append "    1.  A paragraph\n"
                                    "        with two lines.\n"
                                    "\n"
                                    "            indented code\n"
                                    "\n"
                                    "        > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation lines"
             (match (call-with-input-string
                     (string-append "  1.  A paragraph\n"
                                    "with two lines.\n"
                                    "\n"
                                    "          indented code\n"
                                    "\n"
                                    "      > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation lines with partial indentation"
             (match (call-with-input-string
                     (string-append "  1.  A paragraph\n"
                                    "    with two lines.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation in nested structures"
             (match (call-with-input-string
                     (string-append "> 1. > Blockquote\n"
                                    "continued here.")
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('list list-data
                                                ('item item-data
                                                       ('block-quote quote-data2
                                                                     ('paragraph para-data
                                                                                 ('text text-data "Blockquote\ncontinued here.")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation in nested structures"
             (match (call-with-input-string
                     (string-append "> 1. > Blockquote\n"
                                    "> continued here.")
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('list list-data
                                                ('item item-data
                                                       ('block-quote quote-data2
                                                                     ('paragraph para-data
                                                                                 ('text text-data "Blockquote\ncontinued here.")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "  - bar\n"
                                    "    - baz")
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('list list-data3
                                                              ('item item-data3
                                                                     ('paragraph para-data3
                                                                                 ('text text-data3 "baz"))))
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "bar"))
                                                      ))
                                         ('paragraph para-data1
                                                     ('text text-data1 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented, one is not enough"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    " - bar\n"
                                    "  - baz")
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('paragraph para-data1
                                                      ('text text-data1 "baz")))
                                   ('item item-data2
                                          ('paragraph para-data2
                                                      ('text text-data2 "bar")))
                                   ('item item-data3
                                          ('paragraph para-data3
                                                      ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented, need four here"
             (match (call-with-input-string
                     (string-append "10) foo\n"
                                    "    - bar")
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('list list-data2
                                                 ('item item-data2
                                                        ('paragraph para-data1
                                                                    ('text text-data1 "bar"))))
                                          ('paragraph para-data2
                                                      ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented, three is not enough"
             (match (call-with-input-string
                     (string-append "10) foo\n"
                                    "   - bar")
                     parse-blocks)
               (('document doc-data
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data1
                                                      ('text text-data1 "bar"))))
                            ('list list-data1
                                   ('item item-data1
                                          ('paragraph para-data2
                                                      ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may have a list as the first block"
             (match (call-with-input-string
                     "- - foo"
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('list list-data2
                                                 ('item item-data2
                                                        ('paragraph para-data
                                                                    ('text text-data "foo")))))
                                  ))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may have a list as the first block"
             (match (call-with-input-string
                     "1. - 2. foo"
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('list list-data2
                                                 ('item item-data2
                                                        ('list list-data3
                                                               ('item item-data3
                                                                      ('paragraph para-data
                                                                                  ('text text-data "foo")))))))
                                  ))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item can contain a heading"
             (match (call-with-input-string
                     (string-append "- # Foo\n"
                                    "- Bar\n"
                                    "  ---\n"
                                    "  baz")
                     parse-blocks)
               (('document doc-data
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data1
                                                      ('text text-data3 "baz"))
                                          ('heading heading-data1
                                                    ('text text-data1 "Bar")))
                                   ('item item-data1
                                          ('heading heading-data2
                                                    ('text text-data2 "Foo")))))
                #t)
               (x (pk 'fail x #f))))


(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
