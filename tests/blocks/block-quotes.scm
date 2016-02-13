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

(define-module (test-blocks block-quotes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks block-quotes")

(test-assert "parse-blocks, block quote simple"
             (match (call-with-input-string "> # Foo\n> bar\n> baz" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data1 "bar\nbaz"))
                                         ('heading heading-data
                                                   ('text text-data2 "Foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote spaces can be omitted"
             (match (call-with-input-string "># Foo\n>bar\n>baz" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data1 "bar\nbaz"))
                                         ('heading heading-data
                                                   ('text text-data2 "Foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote can be indented 1-3 spaces"
             (match (call-with-input-string "   > # Foo\n   > bar\n > baz" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data1 "bar\nbaz"))
                                         ('heading heading-data
                                                   ('text text-data2 "Foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote paragraph laziness"
             (match (call-with-input-string
                     (string-append "> # Foo\n"
                                    "> bar\n"
                                    "baz")
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data1 "bar\nbaz"))
                                         ('heading heading-data
                                                   ('text text-data2 "Foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote can contain lazy and non-lazy continuation lines"
             (match (call-with-input-string
                     (string-append "> bar\n"
                                    "baz\n"
                                    "> foo")
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data "bar\nbaz\nfoo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote laziness only applies if they would be a paragraph"
             (match (call-with-input-string "> foo\n---" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data "foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote lists are similiar"
             (match (call-with-input-string "> - foo\n- bar" parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))))
                           ('block-quote quote-data
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                              ('text text-data2 "foo"))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote code blocks are similiar"
             (match (call-with-input-string ">     foo\n    bar" parse-blocks)
               (('document doc-data
                           ('code-block code-data1 "bar")
                           ('block-quote quote-data
                                         ('code-block code-data2 "foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote fenced code are similiar"
             (match (call-with-input-string "> ```\nbar\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data1)
                           ('paragraph para-data
                                       ('text text-data "bar"))
                           ('block-quote quote-data
                                         ('fenced-code code-data2)))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote code blocks can't interrupt a paragraph continuation lines"
             (match (call-with-input-string "> foo\n    - bar" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data "foo\n- bar"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote can be empty"
             (match (call-with-input-string ">" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote can be empty with spaces"
             (match (call-with-input-string ">\n>  \n> " parse-blocks)
               (('document doc-data
                           ('block-quote quote-data))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote can have initial or final blank lines"
             (match (call-with-input-string ">\n> foo\n>  " parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data
                                                     ('text text-data "foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote blank lines separates block quote"
             (match (call-with-input-string "> foo\n\n> bar" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                           ('block-quote quote-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote blank lines in a block quote separates paragraph"
             (match (call-with-input-string "> foo\n>\n> bar" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote can interrupt paragraphs"
             (match (call-with-input-string "foo\n> bar" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                           ('paragraph para-data2
                                       ('text text-data2 "foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote blank lines are not needed before or after"
             (match (call-with-input-string "> aaa\n***\n> bbb" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bbb")))
                           ('thematic-break break-data)
                           ('block-quote quote-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "aaa"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote blank line needed to break laziness"
             (match (call-with-input-string "> bar\n\nbaz" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "baz"))
                           ('block-quote quote-data
                                         ('paragraph para-data2
                                                     ('text text-data2 "bar"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote blank line needed to break laziness in block quotes as well"
             (match (call-with-input-string "> bar\n>\nbaz" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "baz"))
                           ('block-quote quote-data
                                         ('paragraph para-data2
                                                     ('text text-data2 "bar"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote laziness rule allowed in nested block quotes"
             (match (call-with-input-string "> > > foo\nbar" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('block-quote quote-data2
                                                       ('block-quote quote-data3
                                                                     ('paragraph para-data
                                                                                 ('text text-data "foo\nbar"))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote laziness rule allowed in nested block quotes of multiple levels"
             (match (call-with-input-string ">>> foo\n> bar\n>>baz" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('block-quote quote-data2
                                                       ('block-quote quote-data3
                                                                     ('paragraph para-data
                                                                                 ('text text-data "foo\nbar\nbaz"))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, block quote code blocks need five spaces"
             (match (call-with-input-string ">     code\n\n>    not code" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "not code")))
                           ('block-quote quote-data2
                                         ('code-block code-data "code")))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
