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

(define-module (test-inlines backslash)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 i18n)
  #:use-module (commonmark inlines)
  #:use-module (commonmark node))

(setlocale LC_ALL "en_US.utf8")

(test-begin "inlines backslash")

(define (make-paragraph text)
  (make-node 'document #f
             (list (make-node 'paragraph #f
                              (list (make-node 'text #f (list text)))))))

(test-assert "parse-inlines, any ASCII punctuation character may be backslashed-escaped"
  (match (parse-inlines (make-paragraph "\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "/")
                            ('text text-data ".")
                            ('text text-data "-")
                            ('text text-data ",")
                            ('text text-data "+")
                            ('text text-data "*")
                            ('text text-data ")")
                            ('text text-data "(")
                            ('text text-data "'")
                            ('text text-data "&")
                            ('text text-data "%")
                            ('text text-data "$")
                            ('text text-data "#")
                            ('text text-data "\"")
                            ('text text-data "!")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, any ASCII punctuation character may be backslash-escaped"
  (match (parse-inlines (make-paragraph "\\:\\;\\<\\=\\>\\?\\@\\[\\]\\^\\_\\`\\{\\|\\}\\~"))
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "~")
                            ('text text-data "}")
                            ('text text-data "|")
                            ('text text-data "{")
                            ('text text-data "`")
                            ('text text-data "_")
                            ('text text-data "^")
                            ('text text-data "]")
                            ('text text-data "[")
                            ('text text-data "@")
                            ('text text-data "?")
                            ('text text-data ">")
                            ('text text-data "=")
                            ('text text-data "<")
                            ('text text-data ";")
                            ('text text-data ":")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, backslashes before other characters are treated as literal backslashes"
  (match (parse-inlines (make-paragraph "\\→\\A\\a\\ \\3\\φ\\«")) 
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "«")
                            ('text text-data "\\")
                            ('text text-data "φ")
                            ('text text-data "\\")
                            ('text text-data "3")
                            ('text text-data "\\")
                            ('text text-data " ")
                            ('text text-data "\\")
                            ('text text-data "a")
                            ('text text-data "\\")
                            ('text text-data "A")
                            ('text text-data "\\")
                            ('text text-data "→")
                            ('text text-data "\\")))
     #t)
    (x (pk 'fail x #f))))


(test-assert "parse-inlines, escaped characters are treated as regular characters and do not have
their usual Markdown meanings"
  (match (parse-inlines (make-paragraph (string-append "\\*not emphasized*\n"
                                                       "\\<http://example.com> not a autolink\n"
                                                       "\\[not a link](/foo)\n"
                                                       "\\`not code`"))) 
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "`")
                            ('text text-data "not code")
                            ('text text-data "`")
                            ('softbreak softbreak-data)
                            ('text text-data "not a link](/foo)")
                            ('text text-data "[")
                            ('softbreak softbreak-data)
                            ('text text-data "http://example.com> not a autolink")
                            ('text text-data "<")
                            ('softbreak softbreak-data)
                            ('text text-data "*")
                            ('text text-data "not emphasized")
                            ('text text-data "*")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, if a backslash is itself escaped, the following character is not"
  (match (parse-inlines (make-paragraph "\\\\*emphasis*")) 
    (('document doc-data
                ('paragraph para-data
                            ('emphasis emphasis-data
                                       ('text text-data "emphasis"))
                            ('text text-data "\\")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, a backslash at the end of the line is a hard line break"
  (match (parse-inlines (make-paragraph "foo\\\nbar")) 
    (('document doc-data
                ('paragraph para-data
                            ('text text-data "bar")
                            ('hardbreak hardbreak-data)
                            ('text text-data "foo")))
     #t)
    (x (pk 'fail x #f))))

(test-assert "parse-inlines, backslash escapes do not work in code spans"
  (match (parse-inlines (make-paragraph "`` \\[\\` ``")) 
    (('document doc-data
                ('paragraph para-data
                            ('code-span code-data "\\[\\`")))
     #t)
    (x (pk 'fail x #f))))

(test-end)
