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

(define (destination=? node-data destination)
  (equal? (assq-ref node-data 'destination) destination))

(define (title=? node-data title)
  (equal? (assq-ref node-data 'title) title))

(test-assert "parse-inlines, simple inline link"
  (match (parse-inlines (make-paragraph "[link](/uri \"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/uri")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link title may be omitted"
  (match (parse-inlines (make-paragraph "[link](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link both the title and the destination may be omitted"
  (match (parse-inlines (make-paragraph "[link]()"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link both the title and the destination may be omitted"
  (match (parse-inlines (make-paragraph "[link](<>)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "")
          (title=? link-data #f)))
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
     (and (destination=? link-data "\\(foo\\)")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link one level of balanced parentheses is allowed without escaping"
  (match (parse-inlines (make-paragraph "[link]((foo)and(bar))"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "(foo)and(bar)")
          (title=? link-data #f)))
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
     (and (destination=? link-data "foo(and\\(bar\\))")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link if you have parentheses within parentheses, you need to
escape or use the <...> form"
  (match (parse-inlines (make-paragraph "[link](<foo(and(bar))>)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "foo(and(bar))")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link parentheses and other symbols can also be escaped"
  (match (parse-inlines (make-paragraph "[link](foo\\)\\:)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "foo\\)\\:")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link can contain fragment identifiers and queries"
  (match (parse-inlines (make-paragraph "[link](#fragment)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "#fragment")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link can contain fragment identifiers and queries"
  (match (parse-inlines (make-paragraph "[link](http://example.com#fragment)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "http://example.com#fragment")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link can contain fragment identifiers and queries"
  (match (parse-inlines (make-paragraph "[link](http://example.com?foo=3#frag)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "http://example.com?foo=3#frag")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link backslash before a non-escapable character is just
a backslash"
  (match (parse-inlines (make-paragraph "[link](foo\\bar)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "foo\\bar")
          (title=? link-data #f)))
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
     (and (destination=? link-data "foo%20b%C3%A4")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles can often be parsed as destinations, if you
try to omit the destination and keep the title, you'll get unexpected results"
  (match (parse-inlines (make-paragraph "[link](\"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "%22title%22")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles may be in double quotes"
  (match (parse-inlines (make-paragraph "[link](/url \"title\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles may be in single quotes"
  (match (parse-inlines (make-paragraph "[link](/url 'title')"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link titles may be in parentheses"
  (match (parse-inlines (make-paragraph "[link](/url (title))"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-inlines, link backslash escapes and entity and numeric character
references may be used in titles"
  (match (parse-inlines (make-paragraph "[link](/url \"title \\\"&quot;\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title &quot;&quot;")))
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
     (and (destination=? link-data "/url")
          (title=? link-data "title \"and\" title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link whitespace is allowed around the destination and title"
  (match (parse-inlines (make-paragraph "[link](   /url\n  \"title\"  )"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "link"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link but it is not allowed between the link text and
the following parenthesis"
  (match (parse-inlines (make-paragraph "[link] (/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "link] (/uri)")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text may contain balanced brackets, but not
unbalanced ones, unless they are escaped"
  (match (parse-inlines (make-paragraph "[link [foo [bar]]](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar]]")
                                   ('text text-data "[")
                                   ('text text-data "foo ")
                                   ('text text-data "[")
                                   ('text text-data "link "))))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text may contain balanced brackets, but not
unbalanced ones, unless they are escaped"
  (match (parse-inlines (make-paragraph "[link] bar](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "link] bar](/uri)")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text may contain balanced brackets, but not
unbalanced ones, unless they are escaped"
  (match (parse-inlines (make-paragraph "[link [bar](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar"))
                            ('text text-data "link ")
                            ('text text-data "[")))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text may contain balanced brackets, but not
unbalanced ones, unless they are escaped"
  (match (parse-inlines (make-paragraph "[link \\[bar](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar")
                                   ('text text-data "[")
                                   ('text text-data "link "))))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(define (em? node-data)
  (eq? (assq-ref node-data 'type) 'em))

(define (strong? node-data)
  (eq? (assq-ref node-data 'type) 'strong))

(test-assert "parse-inlines, link text may contain inline content"
  (match (parse-inlines (make-paragraph "[link *foo **bar** `#`*](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('emphasis em-data1
                                              ('code-span code-data "#")
                                              ('text text-data " ")
                                              (emphasis em-data2
                                                        ('text text-data "bar"))
                                              ('text text-data "foo "))
                                   ('text text-data "link "))))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)
          (em? em-data1)
          (strong? em-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text may contain inline content"
  (match (parse-inlines (make-paragraph "[![moon](moon.jpg)](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('image image-data
                                           ('text text-data "moon")))))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)
          (destination=? image-data "moon.jpg")
          (title=? image-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, links may not contain other links at any level of nesting"
  (match (parse-inlines (make-paragraph "[foo [bar](/uri)](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "](/uri)")
                            ('link link-data
                                   ('text text-data "bar"))
                            ('text text-data "foo ")
                            ('text text-data "[")))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, links may not contain other links at any level of nesting"
  (match (parse-inlines (make-paragraph "[foo *[bar [baz](/uri)](/uri)*](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "](/uri)")
                            ('emphasis em-data
                                       ('text text-data "](/uri)")
                                       ('link link-data
                                              ('text text-data "baz"))
                                       ('text text-data "bar ")
                                       ('text text-data "["))
                            ('text text-data "foo ")
                            ('text text-data "[")))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)
          (em? em-data)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, links may not contain other links at any level of nesting"
  (match (parse-inlines (make-paragraph "![[[foo](uri1)](uri2)](uri3)"))
    (('document doc-data
                ('paragraph para-data
                            ('image image-data
                                    ('text text-data "](uri2)")
                                    ('link link-data
                                           ('text text-data "foo"))
                                    ('text text-data "["))))
     (and (destination=? link-data "uri1")
          (title=? link-data #f)
          (destination=? image-data "uri3")
          (title=? image-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text have higher precedence over emphasis"
  (match (parse-inlines (make-paragraph "*[foo*](/uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "*")
                                   ('text text-data "foo"))
                            ('text text-data "*")))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link text have higher precedence over emphasis"
  (match (parse-inlines (make-paragraph "[foo *bar](baz*)"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar")
                                   ('text text-data "*")
                                   ('text text-data "foo "))))
     (and (destination=? link-data "baz*")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, brackets that aren't part of links do not take
precedence"
  (match (parse-inlines (make-paragraph "*foo [bar* baz]"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data " baz]")
                            ('emphasis em-data
                                   ('text text-data "bar")
                                   ('text text-data "[")
                                   ('text text-data "foo "))))
     (em? em-data))
    (x (pk 'fail x #f))))


(test-expect-fail 1)
(test-assert "parse-inlines, link these cases illustrate the precedence of HTML,
tags, code spans, and autolinks over link grouping"
  (match (parse-inlines (make-paragraph "[foo <bar attr=\"](baz)\">"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data " baz]")
                            ('emphasis em-data
                                   ('text text-data "bar")
                                   ('text text-data "[")
                                   ('text text-data "foo "))))
     (em? em-data))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, link these cases illustrate the precedence of HTML,
tags, code spans, and autolinks over link grouping"
  (match (parse-inlines (make-paragraph "[foo`](/uri)`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "](/uri)")
                            ('text text-data "foo")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-inlines, link these cases illustrate the precedence of HTML,
tags, code spans, and autolinks over link grouping"
  (match (parse-inlines (make-paragraph "[foo<http://example.com/?search=](uri)"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "](/uri)")
                            ('text text-data "foo")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

;; full reference links

(define (make-document text references)
  (node-add-data 
   (make-node 'document '()
              (list (make-node 'paragraph #f
                               (list (make-node 'text #f (list text))))))
   'link-references references))

(test-assert "parse-inlines, full reference link simple"
  (match (parse-inlines (make-document "[foo][bar]"
                                       '(("bar" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link may contain balanced brackets, but not
unbalanced ones, unless they are escaped"
  (match (parse-inlines (make-document "[link [foo [bar]]][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar]]")
                                   ('text text-data "[")
                                   ('text text-data "foo ")
                                   ('text text-data "[")
                                   ('text text-data "link "))))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link may contain balanced brackets, but not
unbalanced ones, unless they are escaped"
  (match (parse-inlines (make-document "[link \\[bar][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar")
                                   ('text text-data "[")
                                   ('text text-data "link "))))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text may contain inline content"
  (match (parse-inlines (make-document "[link *foo **bar** `#`*][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('emphasis em-data1
                                              ('code-span code-data "#")
                                              ('text text-data " ")
                                              ('emphasis em-data2
                                                         ('text text-data "bar"))
                                              ('text text-data "foo "))
                                   ('text text-data "link "))))
     (and (destination=? link-data "/uri")
          (em? em-data1)
          (strong? em-data2)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text may contain inline content"
  (match (parse-inlines (make-document "[![moon](moon.jpg)][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('image image-data
                                           ('text text-data "moon")))))
     (and (destination=? link-data "/uri")
          (title=? link-data #f)
          (destination=? image-data "moon.jpg")
          (title=? image-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text may not contain other links,
at any level of nesting"
  (match (parse-inlines (make-document "[foo [bar](/uri)][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data2
                                   ('text text-data "ref"))
                            ('text text-data "]")
                            ('link link-data1
                                   ('text text-data "bar"))
                            ('text text-data "foo ")
                            ('text text-data "[")))
     (and (destination=? link-data1 "/uri")
          (destination=? link-data2 "/uri")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text may not contain other links,
at any level of nesting"
  (match (parse-inlines (make-document "[foo *bar [baz][ref]*][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data2
                                   ('text text-data "ref"))
                            ('text text-data "]")
                            ('emphasis em-data
                                       ('link link-data
                                              ('text text-data "baz"))
                                       ('text text-data "bar "))
                            ('text text-data "foo ")
                            ('text text-data "[")))
     (and (destination=? link-data "/uri")
          (em? em-data)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text precedence over emphasis grouping"
  (match (parse-inlines (make-document "*[foo*][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "*")
                                   ('text text-data "foo"))
                            ('text text-data "*")))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text precedence over emphasis grouping"
  (match (parse-inlines (make-document "[foo *bar][ref]"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar")
                                   ('text text-data "*")
                                   ('text text-data "foo "))))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-inlines, full reference link text precedence lower than HTML tags,
code spans, and autolinks"
  (match (parse-inlines (make-document "[foo <bar attr=\"][ref]\">"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar")
                                   ('text text-data "*")
                                   ('text text-data "foo "))))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link text precedence lower than HTML tags,
code spans, and autolinks"
  (match (parse-inlines (make-document "[foo`][ref]`"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "][ref]")
                            ('text text-data "foo")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-inlines, full reference link text precedence lower than HTML tags,
code spans, and autolinks"
  (match (parse-inlines (make-document "[foo<http://example.com/?search=][ref]>"
                                       '(("ref" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "][ref]")
                            ('text text-data "foo")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link matching is case-insensitive"
  (match (parse-inlines (make-document "[foo][BaR]"
                                       '(("bar" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

(setlocale LC_ALL "en_US.utf8")
(test-assert "parse-inlines, full reference link unicode case fold is used"
  (match (parse-inlines (make-document "[Толпой][ТОЛПОЙ] is a Russian word."
                                       '(("толпой" "/url" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data " is a Russian word.")
                            ('link link-data
                                   ('text text-data "Толпой"))))
     (destination=? link-data "/url"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link no whitespace is allowed between the link text
and the link label"
  (match (parse-inlines (make-document "[foo] [bar]"
                                       '(("bar" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar"))
                            ('text text-data "foo] ")
                            ('text text-data "[")))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link no whitespace is allowed between the link text
and the link label"
  (match (parse-inlines (make-document "[foo]\n[bar]"
                                       '(("bar" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar"))
                            ('softbreak break-data)
                            ('text text-data "foo]")
                            ('text text-data "[")))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link when there are multiple matching link
reference definitions, the first is used"
  (match (parse-inlines (make-document "[bar][foo]"
                                       '(("foo" "/url1" #f)
                                         ("foo" "/url2" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar"))))
     (destination=? link-data "/url1"))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link matching is performed on normalized strings,
not parsed inline content"
  (match (parse-inlines (make-document "[bar][foo\\!]"
                                       '(("foo!" "/url" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "]")
                            ('text text-data "!")
                            ('text text-data "foo")
                            ('text text-data "[")
                            ('text text-data "bar]")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, full reference link labels cannot contain brackets, unless
they are backslash-escaped"
  (match (parse-inlines (make-document "[foo][ref\\[]"
                                       '(("ref\\[" "/uri" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (destination=? link-data "/uri"))
    (x (pk 'fail x #f))))

;; collapsed reference link

(test-assert "parse-inlines, simple collapsed reference link"
  (match (parse-inlines (make-document "[foo][]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, simple collapsed reference link"
  (match (parse-inlines (make-document "[*foo* bar][]"
                                       '(("*foo* bar" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data " bar")
                                   ('emphasis em-data
                                              ('text text-data "foo")))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")
          (em? em-data)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, collapsed reference link labels are case-insensitive"
  (match (parse-inlines (make-document "[Foo][]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "Foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, collapsed reference link, whitespace is not allowed between
the two sets of brackets"
  (match (parse-inlines (make-document "[foo] \n[]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "]")
                            ('text text-data "[")
                            ('softbreak break-data)
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

;; shortcut reference link

(test-assert "parse-inlines, shortcut reference link simple"
  (match (parse-inlines (make-document "[foo]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link simple"
  (match (parse-inlines (make-document "[*foo* bar]"
                                       '(("*foo* bar" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data " bar")
                                   ('emphasis em-data
                                              ('text text-data "foo")))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")
          (em? em-data)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link simple"
  (match (parse-inlines (make-document "[[*foo* bar]]"
                                       '(("*foo* bar" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "]")
                            ('link link-data
                                   ('text text-data " bar")
                                   ('emphasis em-data
                                              ('text text-data "foo")))
                            ('text text-data "[")))
     (and (destination=? link-data "/url")
          (title=? link-data "title")
          (em? em-data)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link simple"
  (match (parse-inlines (make-document "[[bar [foo]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))
                            ('text text-data "bar ")
                            ('text text-data "[")
                            ('text text-data "[")))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link labels are case-insensitive"
  (match (parse-inlines (make-document "[Foo]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "Foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link a space after the link text should
be preserved"
  (match (parse-inlines (make-document "[foo] bar"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data " bar")
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/url")
          (title=? link-data "title")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link backslash-escape the opening bracket
to avoid links"
  (match (parse-inlines (make-document "\\[foo]"
                                       '(("foo" "/url" "\"title\""))))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "foo]")
                            ('text text-data "[")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link note that this is a link,
because a link label ends with the first following closing bracket"
  (match (parse-inlines (make-document "*[foo*]"
                                       '(("foo*" "/url" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "*")
                                   ('text text-data "foo"))
                            ('text text-data "*")))
     (and (destination=? link-data "/url")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link, full references take precedence
over shortcut references"
  (match (parse-inlines (make-document "[foo][bar]"
                                       '(("foo" "/url1" #f)
                                         ("bar" "/url2" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/url2")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link, in the following case [bar][baz]
is parsed as a reference, [foo] as normal text"
  (match (parse-inlines (make-document "[foo][bar][baz]"
                                       '(("baz" "/url" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar"))
                            ('text text-data "foo]")
                            ('text text-data "[")))
     (and (destination=? link-data "/url")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link, here [foo][bar] is parsed as a reference
since [bar] is defined"
  (match (parse-inlines (make-document "[foo][bar][baz]"
                                       '(("baz" "/url1" #f)
                                         ("bar" "/url2" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data2
                                   ('text text-data "baz"))
                            ('link link-data1
                                   ('text text-data "foo"))))
     (and (destination=? link-data1 "/url2")
          (title=? link-data1 #f)
          (destination=? link-data2 "/url1")
          (title=? link-data2 #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, shortcut reference link, here [foo] is not parsed as a shortcut
reference, because it is followed by a link label (even though [bar] is not defined)"
  (match (parse-inlines (make-document "[foo][bar][baz]"
                                       '(("baz" "/url1" #f)
                                         ("foo" "/url2" #f))))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "bar"))
                            ('text text-data "foo]")
                            ('text text-data "[")))
     (and (destination=? link-data "/url1")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
