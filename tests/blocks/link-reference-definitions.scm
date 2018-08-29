;; Copyright (C) 2016-2018  Erik Edrosa <erik.edrosa@gmail.com>
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

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-64)
             (tests utils))

(test-begin "blocks link-reference-definitions")

(define (link-reference? link-reference document-data)
  (any (cut equal? link-reference <>) (link-references document-data)))

(block-expect "parse-blocks, link reference definition simple"
  "[foo]: /url \"title\"\n\n[foo]"
  ('document doc-data
             ('paragraph _ ('text _ "[foo]")))
  (link-reference? '("foo" "/url" "\"title\"") doc-data) #true)

(block-expect "parse-blocks, link reference definition multiline"
  "   [foo]: \n       /url  \n          'the title'  \n\n[foo]"
  ('document doc-data
             ('paragraph _ ('text _ "[foo]")))
  (link-reference? '("foo" "/url" "'the title'") doc-data) #true)

(block-expect "parse-blocks, link reference definition with parens and escapes"
  "[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]"
  ('document doc-data
             ('paragraph _ ('text _ "[Foo*bar\\]]")))
  (link-reference? '("foo*bar\\]" "my_(url)" "'title (with parens)'") doc-data) #true)


(block-expect "parse-blocks, link reference defitinion"
  "[Foo bar]:\n<my%20url>\n'title'\n\n[Foo bar]"
  ('document doc-data
             ('paragraph _ ('text _ "[Foo bar]")))
  (link-reference? '("foo bar" "my%20url" "'title'") doc-data) #true)

(block-expect "parse-blocks, link reference definition multiline title"
  "[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]"
  ('document doc-data
             ('paragraph _ ('text _ "[foo]")))
  (link-reference? '("foo" "/url" "'\ntitle\nline1\nline2\n'") doc-data) #true)

(block-expect "parse-blocks, link reference definition title may not contain a blank line"
  "[foo]: /url 'title\n\nwith blank line'\n\n[foo]"
  ('document _
             ('paragraph _ ('text _ "[foo]"))
             ('paragraph _ ('text _ "with blank line'"))
             ('paragraph _ ('text _ "[foo]: /url 'title"))))

(block-expect "parse-blocks, link reference definition title may be omitted"
  "[foo]:\n/url\n\n[foo]"
  ('document doc-data
             ('paragraph _ ('text _ "[foo]")))
  (link-reference? '("foo" "/url" #f) doc-data) #true)


(block-expect "parse-blocks, link reference definition link destination may not be omitted"
  "[foo]:\n\n[foo]"
  ('document _
             ('paragraph _ ('text _ "[foo]"))
             ('paragraph _ ('text _ "[foo]:"))))


(block-expect "parse-blocks, link reference definition both title and destination
can contain backslash escapes and literal backslashes"
  "[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]"
  ('document doc-data
             ('paragraph _ ('text _ "[foo]")))
  (link-reference? '("foo" "/url\\bar*baz" "\"foo\"bar\\baz\"") doc-data) #true)

(block-expect "parse-blocks, link reference definition with no corresponding link
contributes nothing to the document."
  "[foo]: /url"
  ('document doc-data))

(block-expect "parse-blocks, link reference definition with no corresponding link
contributes nothing to the document."
  "[\nfoo\n]: /url\nbar"
  ('document _
             ('paragraph _ ('text _ "bar"))))

(block-expect "parse-blocks, link reference definition not a link reference definition"
  "[foo]: /url \"title\" ok"
  ('document _
             ('paragraph _ ('text _ "[foo]: /url \"title\" ok"))))

(block-expect "parse-blocks, link reference definition, but it has no title"
  "[foo]: /url\n\"title\" ok"
  ('document doc-data
             ('paragraph _ ('text _ "\"title\" ok")))
  (link-reference? '("foo" "/url" #f) doc-data) #true)

(block-expect "parse-blocks, link reference definition not a link definition because
indented four spaces"
  "    [foo]: /url \"title\"\n\n[foo]"
  ('document _
             ('paragraph _ ('text _ "[foo]"))
             ('code-block _ "[foo]: /url \"title\"")))

(block-expect "parse-blocks, link reference definition not a link definition because
it occurs inside a code block"
  "```\n[foo]: /url\n```\n\n[foo]"
  ('document _
             ('paragraph _ ('text _ "[foo]"))
             ('fenced-code _ "[foo]: /url")))

(block-expect "parse-blocks, link reference definition cannot interrupt a paragraph"
  "Foo\n[bar]: /baz\n\n[bar]"
  ('document _
             ('paragraph _ ('text _ "[bar]"))
             ('paragraph _ ('text _ "Foo\n[bar]: /baz"))))

(block-expect "parse-blocks, link reference definition can directly follow other blocks"
  "# [Foo]\n[foo]: /url\n> bar"
  ('document doc-data
             ('block-quote _ ('paragraph _ ('text _ "bar")))
             ('heading _ ('text _ "[Foo]")))
  (link-reference? '("foo" "/url" #f) doc-data) #true)

(block-expect "parse-blocks, link reference definition can occur one after another"
  "[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]"
  ('document doc-data
             ('paragraph _ ('text _ "[foo],\n[bar],\n[baz]")))
  (link-reference? '("foo" "/foo-url" "\"foo\"") doc-data) #true
  (link-reference? '("bar" "/bar-url" "\"bar\"") doc-data) #true
  (link-reference? '("baz" "/baz-url" #f) doc-data) #true)

(block-expect "parse-blocks, link reference definition can occur inside block containers"
  "[foo]\n\n> [foo]: /url"
  ('document doc-data
             ('block-quote _)
             ('paragraph _ ('text _ "[foo]")))
  (link-reference? '("foo" "/url" #f) doc-data) #true)

(test-end)
