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

(define-module (test-blocks link-reference-definitions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks link-reference-definitions")

(define (link-references data)
  (assq-ref data 'link-references))

(test-assert "parse-blocks, link reference definition simple"
             (match (call-with-input-string "[foo]: /url \"title\"\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]")))
                (any (cut equal? '("foo" "/url" "\"title\"")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition multiline"
             (match (call-with-input-string
                     "   [foo]: \n       /url  \n          'the title'  \n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]")))
                (any (cut equal? '("foo" "/url" "'the title'")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition with parens and escapes"
             (match (call-with-input-string
                     "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[Foo*bar\\]]")))
                (any (cut equal? '("Foo*bar\\]" "my_(url)" "'title (with parens)'")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, link reference definition multiline title"
             (match (call-with-input-string
                     "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]")))
                (any (cut equal? '("foo" "/url" "'\ntitle\nline1\nline2\n'")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, link reference definition title may not contain a blank line"
             (match (call-with-input-string
                     "[foo]: /url 'title\n\nwith blank line'\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]"))
                           ('paragraph para-data2
                                       ('text text-data2 "with blank line'"))
                           ('paragraph para-data3
                                       ('text text-data3 "[foo]: /url 'title")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition title may be omitted"
             (match (call-with-input-string "[foo]:\n/url\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]")))
                (any (cut equal? '("foo" "/url" #f)  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, link reference definition link destination may not be omitted"
             (match (call-with-input-string "[foo]:\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]"))
                           ('paragraph para-data2
                                       ('text text-data2 "[foo]:")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition not a link reference definition"
             (match (call-with-input-string "[foo]: /url \"title\" ok" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "[foo]: /url \"title\" ok")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition cannot interrupt a paragraph"
             (match (call-with-input-string "Foo\n[bar]: /baz\n\n[bar]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[bar]"))
                           ('paragraph para-data2
                                    ('text text-data2 "Foo\n[bar]: /baz")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition can follow other block elements"
             (match (call-with-input-string "# [Foo]\n[foo]: /url\n> bar" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                           ('heading heading-data
                                    ('text text-data3 "[Foo]")))
                (any (cut equal? '("foo" "/url" #f) <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition several can occur one after another"
             (match (call-with-input-string
                        (string-append "[foo]: /foo-url \"foo\"\n"
                                       "[bar]: /bar-url\n"
                                       "  \"bar\"\n"
                                       "[baz]: /baz-url\n\n"
                                       "[foo],\n"
                                       "[bar],\n"
                                       "[baz]") parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "[foo],\n[bar],\n[baz]")))
                (let ((links (link-references doc-data)))
                  (and (any (cut equal? '("foo" "/foo-url" "\"foo\"") <>) links)
                       (any (cut equal? '("bar" "/bar-url" "\"bar\"") <>) links)
                       (any (cut equal? '("baz" "/baz-url" #f) <>) links))))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition can occur in block container elements"
             (match (call-with-input-string "[foo]\n\n> [foo]: /url" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data)
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]")))
                (any (cut equal? '("foo" "/url" #f) <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
