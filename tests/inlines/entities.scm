;; Copyright (C) 2016, 2018  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (test-inlines entities)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 i18n)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(setlocale LC_ALL "en_US.utf8")

(test-begin "inlines entities")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(test-assert "parse-inlines, simple entity references"
  (match (parse-inlines (make-paragraph (string-append "&nbsp; &amp; &copy; &AElig; &Dcaron;\n"
                                                       "&frac34; &HilbertSpace; &DifferentialD;\n"
                                                       "&ClockwiseContourIntegral; &ngE;")))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "≧̸")
                            ('text text-data " ")
                            ('text text-data "∲")
                            ('softbreak break-data)
                            ('text text-data "ⅆ")
                            ('text text-data " ")
                            ('text text-data "ℋ")
                            ('text text-data " ")
                            ('text text-data "¾")
                            ('softbreak break-data)
                            ('text text-data "Ď")
                            ('text text-data " ")
                            ('text text-data "Æ")
                            ('text text-data " ")
                            ('text text-data "©")
                            ('text text-data " ")
                            ('text text-data "&")
                            ('text text-data " ")
                            ('text text-data "\u00A0")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, simple decimal numeric character references"
  (match (parse-inlines (make-paragraph "&#35; &#1234; &#992; &#98765432; &#0;"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "�")
                            ('text text-data " ")
                            ('text text-data "�")
                            ('text text-data " ")
                            ('text text-data "Ϡ")
                            ('text text-data " ")
                            ('text text-data "Ӓ")
                            ('text text-data " ")
                            ('text text-data "#")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, simple hexadecimal numeric character references"
  (match (parse-inlines (make-paragraph "&#X22; &#XD06; &#xcab;"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "ಫ")
                            ('text text-data " ")
                            ('text text-data "ആ")
                            ('text text-data " ")
                            ('text text-data "\"")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, here are some nonentities"
  (match (parse-inlines (make-paragraph (string-append "&nbsp &x; &#; &#x;\n"
                                                       "&ThisIsNotDefined; &hi?;")))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "hi?;")
                            ('text text-data "&")
                            ('text text-data "ThisIsNotDefined; ")
                            ('text text-data "&")
                            ('softbreak break-data)
                            ('text text-data "#x;")
                            ('text text-data "&")
                            ('text text-data "#; ")
                            ('text text-data "&")
                            ('text text-data "x; ")
                            ('text text-data "&")
                            ('text text-data "nbsp ")
                            ('text text-data "&")))
     #t)
    (x (pk 'fail x #f))))


(define (destination=? node-data destination)
  (equal? (assq-ref node-data 'destination) destination))

(define (title=? node-data title)
  (equal? (assq-ref node-data 'title) title))

(test-assert "parse-inlines, entity and numeric character references are recognized in any context
besides code spans or code blocks, including URLs, link titles, and fenced code block info strings"
  (match (parse-inlines (make-paragraph "[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")"))
    (('document doc-data
                ('paragraph para-data
                            ('link link-data
                                   ('text text-data "foo"))))
     (and (destination=? link-data "/föö")
          (title=? link-data "föö")))
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, entity and numeric character references are treated as literal text in
code spans and code blocks"
  (match (parse-inlines (make-paragraph "`f&ouml;&ouml;`"))
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data
                                       "f&ouml;&ouml;")))
     #t)
    (x (pk 'fail x #f))))

(test-end)
