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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
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

(test-assert "parse-blocks, paragraph leading spaces skipped"
             (match (call-with-input-string "  aaa\n bbb" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "aaa\nbbb")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, paragraph lines after first may ident any ammount"
             (match (call-with-input-string "aaa\n          bbb\n                    ccc" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "aaa\nbbb\nccc")))
                #t)
               (x (pk 'fail x #f))))

(test-equal "parse-blocks, multiline paragraph"
            (call-with-input-string "foo\nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo\nbar"))))

(test-assert "parse-blocks, code block does not interrupt paragraph"
             (match (call-with-input-string "foo\n    bar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "foo\nbar")))
                #t)
               (x (pk 'fail x #f))))

(test-equal "parse-blocks, multiline paragraph preserves line ending spaces"
            (call-with-input-string "foo   \nbar" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "foo   \nbar"))))

(test-assert "parse-blocks, paragraph multiple blank lines have no affect"
            (match (call-with-input-string "aaa\n\nbbb" parse-blocks)
              (('document doc-data
                          ('paragraph para-data1
                                      ('text text-data1 "bbb"))
                          ('paragraph para-data2
                                      ('text text-data2 "aaa")))
               #t)
              (x (pk 'fail x #f))))

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

(test-assert "parse-blocks, atx headings not a heading when # is escaped"
             (match (call-with-input-string "\\## foo" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "\\## foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, atx headings leading and trailing blanks are ignored"
             (match (call-with-input-string "#                  foo                  " parse-blocks)
               (('document doc-data
                           ('heading heading-data
                                       ('text text-data "foo")))
                (eq? (heading-level heading-data) 1))
               (x (pk 'fail x #f))))

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
                                       ('text text-data2 "Foo\n= =")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, setext heading cannot interrupt a paragraph"
             (match (call-with-input-string "Foo\nBar\n---" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('paragraph para-data
                                       ('text text-data "Foo\nBar")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block"
             (match (call-with-input-string "    a simple\n      indented code block" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "a simple\n  indented code block"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block list takes precedence in list item"
             (match (call-with-input-string "1.  foo\n\n    bar" parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data 
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block chunks separated by blank lines"
             (match (call-with-input-string "    chunk1\n\n    chunk2\n  \n \n \n    chunk3" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "chunk1\n\nchunk2\n\n\n\nchunk3"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block includes spaces"
             (match (call-with-input-string "    chunk1\n      \n      chunk2" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "chunk1\n  \n  chunk2"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block fewer than four leading spaces end block"
             (match (call-with-input-string "    foo\nbar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "bar"))
                           ('code-block code-data
                                        "foo"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block and occur before or after blocks that aren't paragraphs"
             (match (call-with-input-string
                     "# Heading\n    foo\nHeading\n------\n    foo\n----" parse-blocks)
               (('document doc-data
                           ('thematic-break break-data)
                           ('code-block code-data "foo")
                           ('heading heading-data1
                                     ('text text-data1 "Heading"))
                           ('code-block code-data "foo")
                           ('heading heading-data2
                                     ('text text-data2 "Heading")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code block first line can have more than four spaces"
             (match (call-with-input-string "        foo\n    bar" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "    foo\nbar"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code blank lines preceding or following are not included"
             (match (call-with-input-string "\n    \n    foo\n    \n\n" parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "foo"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, indented code trailing spaces are included"
             (match (call-with-input-string "    foo  " parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "foo  "))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code simple"
             (match (call-with-input-string "```\n<\n >\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "<\n >"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code simple with tildes"
             (match (call-with-input-string "~~~\n<\n >\n~~~" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "<\n >"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code must use the same character as opening fence"
             (match (call-with-input-string "```\naaa\n~~~\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n~~~"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code must use the same character as opening fence tildes"
             (match (call-with-input-string "````\naaa\n```\n````" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n```"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code unclosed until end of the document"
             (match (call-with-input-string "```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code closes at end of block quote"
             (match (call-with-input-string "> ```\n> aaa\n\nbbb" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "bbb"))
                           ('block-quote quote-data
                                         ('fenced-code code-data
                                                       "aaa")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can have all empty lines"
             (match (call-with-input-string "```\n\n  \n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "\n  "))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can be empty"
             (match (call-with-input-string "```\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can be indented and equivalent spaces removed"
             (match (call-with-input-string " ```\n aaa\naaa\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\naaa"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can be indented and equivalent spaces removed if present"
             (match (call-with-input-string "  ```\naaa\n  aaa\naaa\n  ```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\naaa\naaa"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code closing fence 4 spaces too much"
             (match (call-with-input-string "```\naaa\n    ```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n    ```"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code spaces are not allowed in opening fence"
             (match (call-with-input-string "``` ```\naaa" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "``` ```\naaa")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code spaces are not allowed in closing fence"
             (match (call-with-input-string "~~~~~\naaa\n~~~ ~~" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "aaa\n~~~ ~~"))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code can interrupt paragraphs and followed by paragraphs"
             (match (call-with-input-string "foo\n```\nbar\n```\nbaz" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "baz"))
                           ('fenced-code code-data
                                         "bar")
                           ('paragraph para-data2
                                       ('text text-data2 "foo")))
                #t)
               (x (pk 'fail x #f))))

(define (info-string data)
  (assq-ref data 'info-string))

(test-assert "parse-blocks, fenced code info string"
             (match (call-with-input-string "```scheme\n(define (foo x)\n  3)\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data
                                         "(define (foo x)\n  3)"))
                (equal? (info-string code-data) "scheme"))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code info string empty code"
             (match (call-with-input-string "```;\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data))
                (equal? (info-string code-data) ";"))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, fenced code coding fence cannot have info strings"
             (match (call-with-input-string "```\n``` aaa\n```" parse-blocks)
               (('document doc-data
                           ('fenced-code code-data "``` aaa"))
                (not (equal? (info-string code-data) "aaa")))
               (x (pk 'fail x #f))))

(define (link-references data)
  (assq-ref data 'link-references))

(test-assert "parse-blocks, link reference definition simple"
             (match (call-with-input-string "[foo]: /url \"title\"\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]"))
                           ('paragraph para-data2
                                       ('text text-data2 "")))
                (any (cut equal? '("foo" "/url" "\"title\"")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition multiline"
             (match (call-with-input-string
                     "   [foo]: \n       /url  \n          'the title'  \n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]"))
                           ('paragraph para-data2
                                       ('text text-data2 "")))
                (any (cut equal? '("foo" "/url" "'the title'")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition with parens and escapes"
             (match (call-with-input-string
                     "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[Foo*bar\\]]"))
                           ('paragraph para-data2
                                       ('text text-data2 "")))
                (any (cut equal? '("Foo*bar\\]" "my_(url)" "'title (with parens)'")  <>) (link-references doc-data)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, link reference definition multiline title"
             (match (call-with-input-string
                     "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]"))
                           ('paragraph para-data2
                                       ('text text-data2 "")))
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
                                       ('text text-data1 "[foo]"))
                           ('paragraph para-data2
                                       ('text text-data2 "")))
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
                           ('paragraph para-data2
                                       ('text text-data2 ""))
                           ('heading heading-data
                                    ('text text-data3 "[Foo]")))
                (any (cut equal? '("foo" "/url" #f) <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition several can occur one after another"
             (match (call-with-input-string
                     "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url" parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "")))
                (let ((links (link-references doc-data)))
                  (and (any (cut equal? '("foo" "/foo-url" "\"foo\"") <>) links)
                       (any (cut equal? '("bar" "/bar-url" "\"bar\"") <>) links)
                       (any (cut equal? '("baz" "/baz-url" #f) <>) links))))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, link reference definition can occur in block container elements"
             (match (call-with-input-string "[foo]\n\n> [foo]: /url" parse-blocks)
               (('document doc-data
                           ('block-quote quote-data
                                         ('paragraph para-data2
                                                     ('text text-data2 "")))
                           ('paragraph para-data1
                                       ('text text-data1 "[foo]")))
                (any (cut equal? '("foo" "/url" #f) <>) (link-references doc-data)))
               (x (pk 'fail x #f))))

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

(test-assert "parse-blocks, list item base case"
             (match (call-with-input-string
                     "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     "- one\n\n two"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "two"))
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data2
                                                     ('text text-data2 "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     "- one\n\n  two"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "two"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     " -    one\n\n     two"
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data " two")
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item"
             (match (call-with-input-string
                     " -    one\n\n      two"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data1
                                                     ('text text-data1 "two"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "one")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item not column"
             (match (call-with-input-string
                     "   > > 1.  one\n>>\n>>     two"
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('block-quote quote-data2
                                                       ('list list-data
                                                              ('item item-data
                                                                     ('paragraph para-data1
                                                                                 ('text text-data1 "two"))
                                                                     ('paragraph para-data2
                                                                                 ('text text-data2 "one")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item content must be indented enough to be part of the item not column"
             (match (call-with-input-string
                     ">>- one\n>>\n  >  > two"
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('block-quote quote-data2
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "two"))
                                                       ('list list-data
                                                              ('item item-data
                                                                     ('paragraph para-data2
                                                                                 ('text text-data2 "one")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item needs one space after the list marker"
             (match (call-with-input-string
                     "-one\n\n2.two"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                       ('text text-data1 "2.two"))
                           ('paragraph para-data2
                                       ('text text-data2 "-one")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may not contain blocks with more than one blank line"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "  bar\n"
                                    "\n"
                                    "- foo\n"
                                    "\n"
                                    "\n"
                                    "  bar")
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data4
                                       ('text text-data4 "bar"))
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))
                                  ('item item-data2
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may not contain blocks with more than one blank line except fenced code block"
             (match (call-with-input-string
                     (string-append "- ```\n"
                                    "  foo\n"
                                    "\n"
                                    "\n"
                                    "  bar\n"
                                    "  ```\n"
                                    "\n"
                                    "- baz\n"
                                    "\n"
                                    "  + ```\n"
                                    "    foo\n"
                                    "\n"
                                    "\n"
                                    "    bar\n"
                                    "    ```")
                     parse-blocks)
               (('document doc-data
                           ('list list-data2
                                  ('item item-data4
                                         ('list list-data3
                                                ('item item-data5
                                                       ('fenced-code code-data2 "foo\n\n\nbar")))
                                         ('paragraph para-data5
                                                     ('text text-data5 "baz")))
                                  ('item item-data3
                                         ('fenced-code code-data1 "foo\n\n\nbar"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may contain any kind of block"
             (match (call-with-input-string
                     (string-append "1.  foo\n"
                                    "\n"
                                    "    ```\n"
                                    "    bar\n"
                                    "    ```\n"
                                    "\n"
                                    "    baz\n"
                                    "\n"
                                    "    > bam")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data3
                                                                   ('text text-data3 "bam")))
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))
                                         ('fenced-code code-data "bar")
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item contain indented code blocks that preserve empty lines"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "      bar\n"
                                    "\n"
                                    "      baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data "bar\n\nbaz")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item contain indented code blocks that preserve empty lines unless two blank lines"
             (match (call-with-input-string
                     "- foo

      bar


      baz"
                     parse-blocks)
               (('document doc-data
                           (code-block code-data1 "  baz")
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data2 "bar")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(define (list-start data)
  (assq-ref data 'start))

(test-assert "parse-blocks, list item start numbers must be nine digits or less"
             (match (call-with-input-string
                     "123456789. ok"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "ok")))))
                (equal? (list-start list-data) 123456789))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item start numbers must be nine digits or less"
             (match (call-with-input-string
                     "1234567890. not ok"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "1234567890. not ok")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may begin with 0s"
             (match (call-with-input-string
                     "0. ok"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "ok")))))
                (equal? (list-start list-data) 0))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may begin with 0s"
             (match (call-with-input-string
                     "003. ok"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "ok")))))
                (equal? (list-start list-data) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item number may not be negative"
             (match (call-with-input-string
                     "-1. not ok"
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "-1. not ok")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item code block must be indented four spaces beyond the edge"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "      bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data "bar")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item code block must be indented four spaces beyond the edge"
             (match (call-with-input-string
                     (string-append "  10.  foo\n"
                                    "\n"
                                    "           bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data "bar")
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item if code block is first block, contents must be indented one space after"
             (match (call-with-input-string
                     (string-append "1.     indented code\n"
                                    "\n"
                                    "   paragraph\n"
                                    "\n"
                                    "       more code")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data1 "more code")
                                         ('paragraph para-data
                                                     ('text text-data "paragraph"))
                                         ('code-block code-data2 "indented code"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item if code block is first block, additional space inside code block"
             (match (call-with-input-string
                     (string-append "1.      indented code\n"
                                    "\n"
                                    "   paragraph\n"
                                    "\n"
                                    "       more code")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('code-block code-data1 "more code")
                                         ('paragraph para-data
                                                     ('text text-data "paragraph"))
                                         ('code-block code-data2 " indented code"))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item can start with a blank line"
             (match (call-with-input-string
                     (string-append "-\n"
                                    "  foo\n"
                                    "-\n"
                                    "  ```\n"
                                    "  bar\n"
                                    "  ```\n"
                                    "-\n"
                                    "      baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('code-block code-data1 "baz"))
                                  ('item item-data2
                                         ('fenced-code code-data2 "bar"))
                                  ('item item-data3
                                         ('paragraph para-data
                                                     ('text text-data "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item can begin with at most one blank line"
             (match (call-with-input-string
                     (string-append "-\n"
                                    "\n"
                                    "  foo")
                     parse-blocks)
               (('document doc-data
                           ('paragraph para-data
                                       ('text text-data "foo"))
                           ('list list-data
                                  ('item item-data)))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item empty bullet list item"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "-\n"
                                    "- bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item empty bullet list item space does not matter after list marker"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "-   \n"
                                    "- bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item empty ordered list item"
             (match (call-with-input-string
                      (string-append "1. foo\n"
                                     "2.\n"
                                     "3. bar")
                      parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data2
                                                     ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list may start or end with an empty list item"
             (match (call-with-input-string
                     "*"
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data)))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item indented one space"
             (match (call-with-input-string
                     (string-append " 1.  A paragraph\n"
                                    "     with two lines.\n"
                                    "\n"
                                    "         indented code\n"
                                    "\n"
                                    "     > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, list item indented two spaces"
             (match (call-with-input-string
                     (string-append "  1.  A paragraph\n"
                                    "      with two lines.\n"
                                    "\n"
                                    "          indented code\n"
                                    "\n"
                                    "      > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item indented three spaces"
             (match (call-with-input-string
                     (string-append "   1.  A paragraph\n"
                                    "       with two lines.\n"
                                    "\n"
                                    "           indented code\n"
                                    "\n"
                                    "       > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item indented four spaces gives a code block"
             (match (call-with-input-string
                     (string-append "    1.  A paragraph\n"
                                    "        with two lines.\n"
                                    "\n"
                                    "            indented code\n"
                                    "\n"
                                    "        > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data
                                        "1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote."))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation lines"
             (match (call-with-input-string
                     (string-append "  1.  A paragraph\n"
                                    "with two lines.\n"
                                    "\n"
                                    "          indented code\n"
                                    "\n"
                                    "      > A block quote.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('block-quote quote-data
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "A block quote.")))
                                         ('code-block code-data "indented code")
                                         ('paragraph para-data2
                                                     ('text text-data2 "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation lines with partial indentation"
             (match (call-with-input-string
                     (string-append "  1.  A paragraph\n"
                                    "    with two lines.")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data
                                         ('paragraph para-data
                                                     ('text text-data "A paragraph\nwith two lines.")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation in nested structures"
             (match (call-with-input-string
                     (string-append "> 1. > Blockquote\n"
                                    "continued here.")
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('list list-data
                                                ('item item-data
                                                       ('block-quote quote-data2
                                                                     ('paragraph para-data
                                                                                 ('text text-data "Blockquote\ncontinued here.")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item lazy continuation in nested structures"
             (match (call-with-input-string
                     (string-append "> 1. > Blockquote\n"
                                    "> continued here.")
                     parse-blocks)
               (('document doc-data
                           ('block-quote quote-data1
                                         ('list list-data
                                                ('item item-data
                                                       ('block-quote quote-data2
                                                                     ('paragraph para-data
                                                                                 ('text text-data "Blockquote\ncontinued here.")))))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "  - bar\n"
                                    "    - baz")
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('list list-data3
                                                              ('item item-data3
                                                                     ('paragraph para-data3
                                                                                 ('text text-data3 "baz"))))
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "bar"))
                                                      ))
                                         ('paragraph para-data1
                                                     ('text text-data1 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented, one is not enough"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    " - bar\n"
                                    "  - baz")
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('paragraph para-data1
                                                      ('text text-data1 "baz")))
                                   ('item item-data2
                                          ('paragraph para-data2
                                                      ('text text-data2 "bar")))
                                   ('item item-data3
                                          ('paragraph para-data3
                                                      ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented, need four here"
             (match (call-with-input-string
                     (string-append "10) foo\n"
                                    "    - bar")
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('list list-data2
                                                 ('item item-data2
                                                        ('paragraph para-data1
                                                                    ('text text-data1 "bar"))))
                                          ('paragraph para-data2
                                                      ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item with sublists need to be indented, three is not enough"
             (match (call-with-input-string
                     (string-append "10) foo\n"
                                    "   - bar")
                     parse-blocks)
               (('document doc-data
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data1
                                                      ('text text-data1 "bar"))))
                            ('list list-data1
                                   ('item item-data1
                                          ('paragraph para-data2
                                                      ('text text-data2 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may have a list as the first block"
             (match (call-with-input-string
                     "- - foo"
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('list list-data2
                                                 ('item item-data2
                                                        ('paragraph para-data
                                                                    ('text text-data "foo")))))
                                  ))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item may have a list as the first block"
             (match (call-with-input-string
                     "1. - 2. foo"
                     parse-blocks)
               (('document doc-data
                            ('list list-data1
                                   ('item item-data1
                                          ('list list-data2
                                                 ('item item-data2
                                                        ('list list-data3
                                                               ('item item-data3
                                                                      ('paragraph para-data
                                                                                  ('text text-data "foo")))))))
                                  ))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list item can contain a heading"
             (match (call-with-input-string
                     (string-append "- # Foo\n"
                                    "- Bar\n"
                                    "  ---\n"
                                    "  baz")
                     parse-blocks)
               (('document doc-data
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data1
                                                      ('text text-data3 "baz"))
                                          ('heading heading-data1
                                                    ('text text-data1 "Bar")))
                                   ('item item-data1
                                          ('heading heading-data2
                                                    ('text text-data2 "Foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list changing bullet list delimiter starts a new list"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "- bar\n"
                                    "+ baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data2
                                                      ('text text-data2 "bar")))
                                   ('item item-data3
                                          ('paragraph para-data3
                                                      ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list changing ordered list delimiter starts a new list"
             (match (call-with-input-string
                     (string-append "1. foo\n"
                                    "2. bar\n"
                                    "3) baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                            ('list list-data2
                                   ('item item-data2
                                          ('paragraph para-data2
                                                      ('text text-data2 "bar")))
                                   ('item item-data3
                                          ('paragraph para-data3
                                                      ('text text-data3 "foo")))))
                (eq? (list-start list-data1) 3))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list can interrupt a paragraph"
             (match (call-with-input-string
                     (string-append "Foo\n"
                                    "- bar\n"
                                    "- baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "bar"))))
                            ('paragraph para-data3
                                        ('text text-data3 "Foo")))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list there can be blank lines between items, but two blank lines end a list"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "- bar\n"
                                    "\n"
                                    "\n"
                                    "- baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                           ('list list-data2
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "bar")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines between blocks within a list item also ends a list"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "\n"
                                    "\n"
                                    "  bar\n"
                                    "- baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))))
                            ('paragraph para-data2
                                        ('text text-data2 "bar"))
                           ('list list-data2
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines end all containing lists"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "  - bar\n"
                                    "    - baz\n"
                                    "\n"
                                    "\n"
                                    "      bim")
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data "  bim")
                           ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('list list-data3
                                                              ('item item-data3
                                                                     ('paragraph para-data1
                                                                                 ('text text-data1 "baz"))))
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "bar"))))
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines can separate consecutive lists of the same type"
             (match (call-with-input-string
                     (string-append "- foo\n"
                                    "- bar\n"
                                    "\n"
                                    "\n"
                                    "- baz\n"
                                    "- bim")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bim")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "baz"))))
                           ('list list-data2
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "bar")))                               
                                  ('item item-data4
                                         ('paragraph para-data4
                                                     ('text text-data4 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list two blank lines can separate an indented code block that would be a paragraph"
             (match (call-with-input-string
                     (string-append "-   foo\n"
                                    "\n"
                                    "    notcode\n"
                                    "\n"
                                    "-   foo\n"
                                    "\n"
                                    "\n"
                                    "    code")
                     parse-blocks)
               (('document doc-data
                           ('code-block code-data "code")
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "foo")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "notcode"))
                                         ('paragraph para-data3
                                                     ('text text-data3 "foo")))))
                #t)
               (x (pk 'fail x #f))))

(test-expect-fail 2)
(test-assert "parse-blocks, list need not be indented to the same level"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    " - b\n"
                                    "  - c\n"
                                    "   - d\n"
                                    "    - e\n"
                                    "   - f\n"
                                    "  - g\n"
                                    " - h\n"
                                    "- i")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "i")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "h")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "g")))
                                  ('item item-data4
                                         ('paragraph para-data4
                                                     ('text text-data4 "f")))
                                  ('item item-data5
                                         ('paragraph para-data5
                                                     ('text text-data5 "e")))
                                  ('item item-data6
                                         ('paragraph para-data6
                                                     ('text text-data6 "d")))
                                  ('item item-data7
                                         ('paragraph para-data7
                                                     ('text text-data7 "c")))
                                  ('item item-data8
                                         ('paragraph para-data8
                                                     ('text text-data8 "b")))
                                  ('item item-data9
                                         ('paragraph para-data9
                                                     ('text text-data9 "a")))))
                #t)
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list need not be indented to the same level"
             (match (call-with-input-string
                     (string-append "1. a\n"
                                    "\n"
                                    "  2. b\n"
                                    "\n"
                                    "    3. c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "b")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "a")))))
                #t)
               (x (pk 'fail x #f))))

(define (list-tight? data)
  (assq-ref data 'tight))

(test-expect-fail 4)
(test-assert "parse-blocks, list with a blank line between items is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- b\n"
                                    "\n"
                                    "- c\n")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "b")))
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))


(test-assert "parse-blocks, list with a blank line between items is loose"
             (match (call-with-input-string
                     (string-append "* a\n"
                                    "*\n"
                                    "\n"
                                    "* c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2)
                                  ('item item-data3
                                         ('paragraph para-data3
                                                     ('text text-data3 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list with a list item with two block level elements is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- b\n"
                                    "\n"
                                    "  c\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 "c"))
                                         ('paragraph para-data3
                                                     ('text text-data3 "b")))
                                  ('item item-data3
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list with a list item with two block level elements is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- b\n"
                                    "\n"
                                    "  [ref]: /url\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data2
                                         ('paragraph para-data2
                                                     ('text text-data2 ""))
                                         ('paragraph para-data3
                                                     ('text text-data3 "b")))
                                  ('item item-data3
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (not (list-tight? list-data)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list with blank lines in code block are tight"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "- ```\n"
                                    "  b\n"
                                    "\n"
                                    "\n"
                                    "  ```\n"
                                    "- c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data2
                                         ('fenced-code code-data "b\n\n"))
                                  ('item item-data3
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data))
               (x (pk 'fail x #f))))

(test-expect-fail 1)
(test-assert "parse-blocks, list is tight even if a sublist is loose"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  - b\n"
                                    "\n"
                                    "    c\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data3
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "c"))
                                                       ('paragraph para-data3
                                                                   ('text text-data3 "b"))))
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list is tight if blank line is inside a block quote"
             (match (call-with-input-string
                     (string-append "* a\n"
                                    "  > b\n"
                                    "  >\n"
                                    "* c")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "c")))
                                  ('item item-data3
                                         ('block-quote quote-data
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "b")))
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list is tight if the consecutive block elements are not separated by blank lines"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  > b\n"
                                    "  ```\n"
                                    "  c\n"
                                    "  ```\n"
                                    "- d")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data3
                                         ('fenced-code code-data "c")
                                         ('block-quote quote-data
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "b")))
                                         ('paragraph para-data4
                                                     ('text text-data4 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list a single paragraph is tight"
             (match (call-with-input-string
                     "- a"
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "a")))))
                (list-tight? list-data1))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, list a single paragraph is tight"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  - b")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                                   ('text text-data1 "b"))))
                                         ('paragraph para-data1
                                                     ('text text-data1 "a")))))
                (and (list-tight? list-data1)
                     (list-tight? list-data2)))
               (x (pk 'fail x #f))))

(test-expect-fail 3)
(test-assert "parse-blocks, list is loose if blank line between two block elements"
             (match (call-with-input-string
                     (string-append "1. ```\n"
                                    "   foo\n"
                                    "   ```\n"
                                    "\n"
                                    "   bar")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "bar"))
                                         ('fenced-code code-data "foo"))))
                (not (list-tight? list-data1)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, outer list is loose and inner list tight"
             (match (call-with-input-string
                     (string-append "* foo\n"
                                    "  * bar\n"
                                    "\n"
                                    "  baz")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('paragraph para-data1
                                                     ('text text-data1 "baz"))
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data1
                                                                   ('text text-data1 "bar"))))
                                         ('paragraph para-data1
                                                     ('text text-data1 "foo"))
                                        )))
                (and (not (list-tight? list-data1))
                     (list-tight? list-data2)))
               (x (pk 'fail x #f))))

(test-assert "parse-blocks, outer list is loose and inner list tight"
             (match (call-with-input-string
                     (string-append "- a\n"
                                    "  - b\n"
                                    "  - c\n"
                                    "\n"
                                    "- d\n"
                                    "  - e\n"
                                    "  - f")
                     parse-blocks)
               (('document doc-data
                           ('list list-data1
                                  ('item item-data1
                                         ('list list-data2
                                                ('item item-data2
                                                       ('paragraph para-data2
                                                                   ('text text-data2 "f")))
                                                ('item item-data3
                                                       ('paragraph para-data3
                                                                   ('text text-data3 "e"))))
                                         ('paragraph para-data1
                                                     ('text text-data1 "d")))
                                  ('item item-data4
                                         ('list list-data3
                                                ('item item-data4
                                                       ('paragraph para-data4
                                                                   ('text text-data4 "c")))
                                                ('item item-data5
                                                       ('paragraph para-data5
                                                                   ('text text-data5 "b"))))
                                         ('paragraph para-data6
                                                     ('text text-data6 "a")))))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
