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

(define-module (test-inlines hardbreak)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))


(test-begin "inlines hardbreak")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(test-assert "parse-inlines, A line break that is preceded by two or more spaces
and does not occur at the end of a block is parsed as a hard line break"
  (match (parse-inlines (make-paragraph "foo  \nbaz"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "baz")
                            ('hardbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, a backslash before the line ending may be used
instead of two spaces"
  (match (parse-inlines (make-paragraph "foo\\\nbaz"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "baz")
                            ('hardbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, hardbreak more than two spaces can be used"
  (match (parse-inlines (make-paragraph "foo       \nbaz"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "baz")
                            ('hardbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, hardbreak leading spaces at the beginning of the next
are ignored"
  (match (parse-inlines (make-paragraph "foo  \n     bar"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "bar")
                            ('hardbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, hardbreak leading spaces at the beginning of the next
are ignored"
  (match (parse-inlines (make-paragraph "foo\\\n     bar"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "bar")
                            ('hardbreak break-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, line breaks can occur inside emphasis, links, and other
constructs that allow inline content"
  (match (parse-inlines (make-paragraph "*foo  \nbar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "bar")
                                       ('hardbreak break-data)
                                       ('text text-data "foo"))))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, line breaks can occur inside emphasis, links, and other
constructs that allow inline content"
  (match (parse-inlines (make-paragraph "*foo\\\nbar*"))
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "bar")
                                       ('hardbreak break-data)
                                       ('text text-data "foo"))))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, line breaks do not occur inside code spans"
  (match (parse-inlines (make-paragraph "`code  \nspan`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "code span")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, line breaks do not occur inside code spans"
  (match (parse-inlines (make-paragraph "`code\\\nspan`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "code\\ span")))
     #t)
    (x (pk 'fail x #f))))

(test-expect-fail 2)
(test-assert "parse-inlines, line breaks do not occur inside html tags"
  (match (parse-inlines (make-paragraph "<a href=\"foo  \nbar\">"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "code span")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, line breaks do not occur inside html tags"
  (match (parse-inlines (make-paragraph "<a href=\"foo\\\nbar\">"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "code\\ span")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, hard line breaks do not work at the end of a paragraph or other
block element"
  (match (parse-inlines (make-paragraph "foo\\"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "\\")
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, hard line breaks do not work at the end of a paragraph or other
block element"
  (match (parse-inlines (make-paragraph "foo  "))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))


(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
