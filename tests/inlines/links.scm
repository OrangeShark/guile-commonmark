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

(define-module (test-inlines links)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(test-begin "inlines links")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(define (link-destination=? node-data destination)
  (equal? (assq-ref node-data 'destination) destination))

(define (link-title=? node-data title)
  (equal? (assq-ref node-data 'title) title))

(test-assert "parse-inlines, simple inline link"
  (match (parse-inlines (make-paragraph "[link](/uri \"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/uri")
          (link-title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link title may be omitted"
  (match (parse-inlines (make-paragraph "[link](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/uri")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link both the title and the destination may be omitted"
  (match (parse-inlines (make-paragraph "[link]()"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link both the title and the destination may be omitted"
  (match (parse-inlines (make-paragraph "[link](<>)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link the destination cannot contain spaces, even if enclosed in
pointy brackets"
  (match (parse-inlines (make-paragraph "[link](/my uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "link](/my uri)")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link the destination cannot contain spaces, even if enclosed in
pointy brackets"
  (match (parse-inlines (make-paragraph "[link](</my uri>)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "link](</my uri>)")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link the destination cannot contain line breaks, even if enclosed in
pointy brackets"
  (match (parse-inlines (make-paragraph "[link](foo\nbar)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "bar)")
                            ('softbreak break-data)
                            ('text text-data "link](foo")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link the destination cannot contain line breaks, even if enclosed in
pointy brackets"
  (match (parse-inlines (make-paragraph "[link](<foo\nbar>)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "bar>)")
                            ('softbreak break-data)
                            ('text text-data "link](<foo")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link parentheses inside the link destination may be escaped"
  (match (parse-inlines (make-paragraph "[link](\\(foo\\))"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "\\(foo\\)")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link one level of balanced parentheses is allowed without escaping"
  (match (parse-inlines (make-paragraph "[link]((foo)and(bar))"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "(foo)and(bar)")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link if you have parentheses within parentheses, you need to
escape or use the <...> form"
  (match (parse-inlines (make-paragraph "[link](foo(and(bar)))"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "link](foo(and(bar)))")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link if you have parentheses within parentheses, you need to
escape or use the <...> form"
  (match (parse-inlines (make-paragraph "[link](foo(and\\(bar\\)))"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "foo(and\\(bar\\))")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link if you have parentheses within parentheses, you need to
escape or use the <...> form"
  (match (parse-inlines (make-paragraph "[link](<foo(and(bar))>)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "foo(and(bar))")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link parentheses and other symbols can also be escaped"
  (match (parse-inlines (make-paragraph "[link](foo\\)\\:)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "foo\\)\\:")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link can contain fragment identifiers and queries"
  (match (parse-inlines (make-paragraph "[link](#fragment)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "#fragment")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link can contain fragment identifiers and queries"
  (match (parse-inlines (make-paragraph "[link](http://example.com#fragment)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "http://example.com#fragment")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link can contain fragment identifiers and queries"
  (match (parse-inlines (make-paragraph "[link](http://example.com?foo=3#frag)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "http://example.com?foo=3#frag")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link backslash before a non-escapable character is just
a backslash"
  (match (parse-inlines (make-paragraph "[link](foo\\bar)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "foo\\bar")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-expect-fail 2)
(test-assert "parse-inlines, link url-escaping should be left alone and entity and
numerical character references in the destination will be parsed into the corresponding
Unicode code points"
  (match (parse-inlines (make-paragraph "[link](foo%20b&auml;)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "foo%20b%C3%A4")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles can often be parsed as destinations, if you
try to omit the destination and keep the title, you'll get unexpected results"
  (match (parse-inlines (make-paragraph "[link](\"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "%22title%22")
          (link-title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles may be in double quotes"
  (match (parse-inlines (make-paragraph "[link](/url \"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/url")
          (link-title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles may be in single quotes"
  (match (parse-inlines (make-paragraph "[link](/url 'title')"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/url")
          (link-title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-expect-fail 2)
(test-assert "parse-inlines, link titles may be in parentheses"
  (match (parse-inlines (make-paragraph "[link](/url (title))"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/url")
          (link-title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link backslash escapes and entity and numeric character
references may be used in titles"
  (match (parse-inlines (make-paragraph "[link](/url \"title \\\"&quot;\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/url")
          (link-title=? link-data "title &quot;&quot;")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link nested balanced quotes are not allowed without escaping"
  (match (parse-inlines (make-paragraph "[link](/url \"title \"and\" title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "link](/url \"title \"and\" title\")")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link but it is easy to work around this by using a different quote type"
  (match (parse-inlines (make-paragraph "[link](/url 'title \"and\" title')"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/url")
          (link-title=? link-data "title \"and\" title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link whitespace is allowed around the destination and title"
  (match (parse-inlines (make-paragraph "[link](   /url\n  \"title\"  )"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (link-destination=? link-data "/url")
          (link-title=? link-data "title")))
    (x (pk 'fail x #f))))


(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
