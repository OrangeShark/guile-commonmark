;; Copyright (C) 2017  Erik Edrosa <erik.edrosa@gmail.com>
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
             (ice-9 match)
             (commonmark blocks)
             (tests utils))

(test-begin "tabs")

(block-expect "parse atx headings with tabs"
  "#\tfoo\t#\t"
  ('document _ 
             ('heading heading-data
                       ('text _ "foo")))
  (heading-level heading-data) 1)

(block-expect "parse setext heading ending with tab"
  "foo
===\t"
  ('document _
             ('heading heading-data
                       ('text _ "foo")))
  (heading-level heading-data) 1)

(block-expect "parse thematic breaks with tabs"
  "*\t*\t*\t"
  ('document _
             ('thematic-break _)))


(block-expect "parse link reference definition with tabs"
  "[foo]:\t/url\t'the title'\t\n\n[foo]"
  ('document doc-data 
             ('paragraph _
                         (text _ "[foo]")))
  (any (cut equal? '("foo" "/url" "'the title'") <>) (link-references doc-data))
  #t)

(block-expect "parse code block preserves tabs"
  "\tfoo\tbaz\t\tbim"
  ('document _
             ('code-block _ "foo\tbaz\t\tbim")))


(block-expect "parse code block preserves tabs and expands"
  "  \tfoo\tbaz\t\tbim"
  ('document _
             ('code-block _ "foo\tbaz\t\tbim")))

(block-expect "parse block quote with code block with tabs"
  ">\t\tfoo"
  ('document _
             ('block-quote _
                           ('code-block _ "  foo"))))

(block-expect "parse continuation paragraph in a list"
  "  - foo

\tbar"
  ('document _
             ('list _
                    ('list-item _
                                ('paragraph _ ('text _ "bar"))
                                ('paragraph _ ('text _ "foo"))))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
