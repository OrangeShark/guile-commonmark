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

(define-module (test-inlines autolinks)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(test-begin "inlines autolinks")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(define (destination=? node-data destination)
  (equal? (assq-ref node-data 'destination) destination))

(define (title=? node-data title)
  (equal? (assq-ref node-data 'title) title))

(test-assert "parse-inlines, simple autolinks"
  (match (parse-inlines (make-paragraph "<http://foo.bar.baz>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "http://foo.bar.baz"))))
     (and (destination=? link-data "http://foo.bar.baz")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, simple autolinks"
  (match (parse-inlines (make-paragraph "<http://foo.bar.baz/test?q=hello&id=22&boolean>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "http://foo.bar.baz/test?q=hello&id=22&boolean"))))
     (and (destination=? link-data "http://foo.bar.baz/test?q=hello&id=22&boolean")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, simple autolinks"
  (match (parse-inlines (make-paragraph "<irc://foo.bar:2233/baz>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "irc://foo.bar:2233/baz"))))
     (and (destination=? link-data "irc://foo.bar:2233/baz")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, autolinks uppercase is also fine"
  (match (parse-inlines (make-paragraph "<MAILTO:FOO@BAR.BAZ>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "MAILTO:FOO@BAR.BAZ"))))
     (and (destination=? link-data "MAILTO:FOO@BAR.BAZ")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, autolinks note that many strings that count as absolute URIs for 
purposes of this spec are not valid URIs"
  (match (parse-inlines (make-paragraph "<a+b+c:d>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "a+b+c:d"))))
     (and (destination=? link-data "a+b+c:d")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, autolinks note that many strings that count as absolute URIs for
purposes of this spec are not valid URIs"
  (match (parse-inlines (make-paragraph "<made-up-scheme://foo,bar>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "made-up-scheme://foo,bar"))))
     (and (destination=? link-data "made-up-scheme://foo,bar")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, autolinks note that many strings that count as absolute URIs for
purposes of this spec are not valid URIs"
  (match (parse-inlines (make-paragraph "<http://../>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "http://../"))))
     (and (destination=? link-data "http://../")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, autolinks note that many strings that count as absolute URIs for
purposes of this spec are not valid URIs"
  (match (parse-inlines (make-paragraph "<localhost:5001/foo>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                       ('text text-data "localhost:5001/foo"))))
     (and (destination=? link-data "localhost:5001/foo")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, autolinks spaces are not allowed"
  (match (parse-inlines (make-paragraph "<http://foo.bar/baz bim>"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "http://foo.bar/baz bim>")
                            ('text text-data "<")))
     #t)
    (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-inlines, autolinks backslash-escpaes do not work inside autolinks"
  (match (parse-inlines (make-paragraph "<http://example.com/\\[\\>"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "http://example.com/\\[\\"))))
     (and (destination=? link-data "http://example.com/%5C%5B%5C")
          (title=? link-data #f)))
    (x (pk 'fail x #f))))


(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
