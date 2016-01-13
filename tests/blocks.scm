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

(define-module (test-blocks)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks")

(test-equal "parse-blocks, empty document"
            (call-with-input-string "" parse-blocks)
            '(document ((closed . #f))))

(test-equal "parse-blocks, simple paragraph"
            (call-with-input-string "foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-equal "parse-blocks, three space simple paragraph"
            (call-with-input-string "   foo" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo"))))

(test-equal "parse-blocks, multiline paragraph"
            (call-with-input-string "foo\nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "bar" "foo"))))

(test-assert "parse-blocks, code block does not interrupt paragraph"
             (match (call-with-input-string "foo\n    bar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "bar" "foo")))
                #t)
               (x (pk 'fail x #f))))

(test-equal "parse-blocks, multiline paragraph preserves line ending spaces"
            (call-with-input-string "foo   \nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "bar" "foo   "))))

(test-equal "parse-blocks, thematic breaks with *"
            (call-with-input-string "***\n **  * ** * ** * **" parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks with -"
            (call-with-input-string "---\n - - -" parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks with _"
            (call-with-input-string "___\n   _______________________  " parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks must not have other characters"
            (call-with-input-string "---a---" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "---a---"))))

(test-assert "parse-blocks, thematic breaks can interupt a paragraph"
             (match (call-with-input-string "Foo\n***\nbar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                      ('text text-data1 "bar"))
                           ('thematic-break (('closed . #t)))
                           ('paragraph para-data2
                                      ('text text-data2 "Foo")))
                #t)
               (x (pk 'fail x #f))))

(define (heading-level heading-data)
  (assq-ref heading-data 'level))

(test-assert "parse-blocks, atx headings"
             (match (call-with-input-string
                     "# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo" parse-blocks)
               (('document doc-data
                           ('heading heading-data6
                                    ('text text-data6 "foo"))
                           ('heading heading-data5
                                    ('text text-data5 "foo"))
                           ('heading heading-data4
                                    ('text text-data4 "foo"))
                           ('heading heading-data3
                                    ('text text-data3 "foo"))
                           ('heading heading-data2
                                    ('text text-data2 "foo"))
                           ('heading heading-data1
                                    ('text text-data1 "foo")))
                (and (eq? (heading-level heading-data6) 6)
                     (eq? (heading-level heading-data5) 5)
                     (eq? (heading-level heading-data4) 4)
                     (eq? (heading-level heading-data3) 3)
                     (eq? (heading-level heading-data2) 2)
                     (eq? (heading-level heading-data1) 1)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings not more than 6 #"
             (match (call-with-input-string "####### foo" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                      ('text text-data "####### foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings requires an empty space"
             (match (call-with-input-string "#hashtag" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "#hashtag")))
                #t)
               (x (pk 'fail x #f))))

(test-expect-fail 2)
(test-assert "parse-blocks, atx headings closing # characters are optional"
             (match (call-with-input-string "## foo ##\n  ###   bar    ###" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 "bar"))
                           ('heading heading-data2
                                     ('text text-data2 "foo")))
                (and (eq? (heading-level heading-data1) 3)
                     (eq? (heading-level heading-data2) 2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings spaces are allowed after closing sequence"
             (match (call-with-input-string "### foo ###     " parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                     ('text text-data "foo")))
                (eq? (heading-level heading-data) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings nonspace character after closing sequence"
             (match (call-with-input-string "### foo ### b" parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                     ('text text-data "foo ### b")))
                (eq? (heading-level heading-data) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings closing sequence must be preceded by a space"
             (match (call-with-input-string "# foo#" parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                     ('text text-data "foo#")))
                (eq? (heading-level heading-data) 1))
               (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-blocks, atx headings can be empty"
             (match (call-with-input-string "## \n#\n### ###" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 ""))
                           ('heading heading-data2
                                     ('text text-data2 ""))
                           ('heading heading-data3
                                     ('text text-data3 "")))
                (and (eq? (heading-level heading-data1) 3)
                     (eq? (heading-level heading-data2) 1)
                     (eq? (heading-level heading-data3) 2)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, setext headings"
             (match (call-with-input-string "Foo\n-------------------------\n\nBar\n=" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 "Bar"))
                           ('heading heading-data2
                                     ('text text-data2 "Foo")))
                (and (eq? (heading-level heading-data1) 1)
                     (eq? (heading-level heading-data2) 2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext headings up to three space indents"
             (match (call-with-input-string
                     "   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===" parse-blocks)
               (('document doc-data
                           ('heading heading-data1
                                     ('text text-data1 "Foo"))
                           ('heading heading-data2
                                     ('text text-data2 "Foo"))
                           ('heading heading-data3
                                     ('text text-data3 "Foo")))
                (and (eq? (heading-level heading-data1) 1)
                     (eq? (heading-level heading-data2) 2)
                     (eq? (heading-level heading-data3) 2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext headings four space indents too much"
             (match (call-with-input-string
                     "    Foo\n    ---\n\n    Foo\n---" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('code-block code-data "Foo\n---\n\nFoo"))
                #t)
               (x (pk 'fail x #f))))


(test-expect-fail 1)
(test-assert "parse-blocks, setext headings four space indent underline too much"
             (match (call-with-input-string "Foo\n    ---" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "Foo\n---")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext headings underline cannot contain interal spaces"
             (match (call-with-input-string "Foo\n= =\n\nFoo\n--- -" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('paragraph para-data1
                                       ('text text-data1 "Foo"))
                           ('paragraph para-data2
                                       ('text text-data2 "= =" "Foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext heading cannot interrupt a paragraph"
             (match (call-with-input-string "Foo\nBar\n---" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('paragraph para-data
                                       ('text text-data "Bar" "Foo")))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
