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

(define-module (test-inlines code-spans)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(test-begin "inlines code-spans")

(define (make-paragraph text)
  (make-node 'document #f 
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))


(test-assert "parse-inlines, simple code span"
  (match (parse-inlines (make-paragraph "`foo`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data 
                                        "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, two backs ticks"
  (match (parse-inlines (make-paragraph "`` foo ` bar  ``"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                        "foo ` bar")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans stripping leading and trailing spaces"
  (match (parse-inlines (make-paragraph "` `` `"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                        "``")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans line endings are treated like spaces"
  (match (parse-inlines (make-paragraph "``\nfoo\n``"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                        "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans interior spaces and line endings are collasped into single spaces"
  (match (parse-inlines (make-paragraph "`foo   bar\n  baz`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                        "foo bar baz")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans with two ticks inside"
  (match (parse-inlines (make-paragraph "`foo `` bar`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                        "foo `` bar")))
     #t)
    (x (pk 'fail x #f))))


(test-assert "parse-inlines, backslash escapes do not work in code spans"
  (match (parse-inlines (make-paragraph "`foo\\`bar`"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data
                                   "bar`")
                            ('code-span code-data
                                        "foo\\")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans backticks have higher precedence, emphasized text"
  (match (parse-inlines (make-paragraph "*foo`*`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                        "*")
                            ('text text-data
                                   "*foo`")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans backticks have higher precedence, link"
  (match (parse-inlines (make-paragraph "[not a `link](/foo`)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2
                                   ")")
                            ('code-span code-data
                                        "link](/foo")
                            ('text text-data
                                   "[not a")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans have same precedence as HTML tags, code"
  (match (parse-inlines (make-paragraph "`<a href=\"`\">`"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2
                                   "\">`")
                            ('code-span code-data
                                        "<a href=\"")))
     #t)
    (x (pk 'fail x #f))))

;; TODO HTML tag
(test-expect-fail 1)
(test-assert "parse-inlines, code spans have same precedence as HTML tags, HTML tag"
  (match (parse-inlines (make-paragraph "<a href=\"`\">`"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2
                                   "\">`")
                            ('code-span code-data
                                        "<a href=\"")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, code spans have same precedence as autolink, code"
  (match (parse-inlines (make-paragraph "`<http://foo.bar.`baz>`"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2
                                   "baz>`")
                            ('code-span code-data
                                        "<http://foo.bar.")))
     #t)
    (x (pk 'fail x #f))))

;; TODO autolink
(test-expect-fail 1)
(test-assert "parse-inlines, code spans have same precedence as autolink, autolink"
  (match (parse-inlines (make-paragraph "<http://foo.bar.`baz>`"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2
                                   "baz>`")
                            ('code-span code-data
                                        "<http://foo.bar.")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, backticks with no matching backtick are literal backticks"
  (match (parse-inlines (make-paragraph "```foo``"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data3
                                   "``")
                            ('text text-data2
                                   "foo")
                            ('text text-data1
                                   "```")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, backticks with no matching backtick are literal backticks"
  (match (parse-inlines (make-paragraph "`foo"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data2
                                   "foo")
                            ('text text-data1
                                   "`")))
     #t)
    (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
