;; Copyright (C) 2016, 2017  Erik Edrosa <erik.edrosa@gmail.com>
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

(use-modules (srfi srfi-64)
             (tests utils))

(test-begin "blocks setext-headings")


(block-expect "parse-blocks, simple setext headings"
  "Foo *bar*
=========

Foo *bar*
---------"
  ('document _
             ('heading heading-data1 ('text _ "Foo *bar*"))
             ('heading heading-data2 ('text _ "Foo *bar*")))
  (heading-level heading-data1) 2
  (heading-level heading-data2) 1)

(test-expect-fail 1) ;;TODO implement multiline headings
(block-expect "parse-blocks, setext headings may span more than one line"
  "Foo *bar
baz*
===="
  ('document _
             ('heading heading-data ('text _ "Foo *bar\nbaz*")))
  (heading-level heading-data) 1)

(block-expect "parse-blocks, setext heading underline can be any length"
  "Foo
-------------------------

Foo
="
  ('document _
             ('heading heading-data1 ('text _ "Foo"))
             ('heading heading-data2 ('text _ "Foo")))
  (heading-level heading-data1) 1
  (heading-level heading-data2) 2)

(block-expect "parse-blocks, setext heading content can be indented up to three spaces"
  "   Foo
---

  Foo
-----

  Foo
  ==="
  ('document _
             ('heading heading-data1 ('text _ "Foo"))
             ('heading heading-data2 ('text _ "Foo"))
             ('heading heading-data3 ('text _ "Foo")))
  (heading-level heading-data1) 1
  (heading-level heading-data2) 2
  (heading-level heading-data3) 2)

(block-expect "parse-blocks, setext heading four spaces indent is too much"
  "    Foo
    ---

    Foo
---"
  ('document _
             ('thematic-break _)
             ('code-block _ "Foo\n---\n\nFoo")))

(block-expect "parse-blocks, setext heading underline can be indented up to three spaces
and may have trailing spaces"
  "Foo
   ----      "
  ('document _
             ('heading heading-data ('text _ "Foo")))
  (heading-level heading-data) 2)

(block-expect "parse-blocks, setext heading underline four spaces it too much"
  "Foo
    ---"
  ('document _
             ('paragraph _ ('text _ "Foo\n---"))))

(block-expect "parse-blocks, setext heading underline cannot contain internal spaces"
  "Foo
= =

Foo
--- -"
  ('document _
             ('thematic-break _)
             ('paragraph _ ('text _ "Foo"))
             ('paragraph _ ('text _ "Foo\n= ="))))

(block-expect "parse-blocks, setext heading underline cannot be a lazy continuation line"
  "> Foo
---"
  ('document _
             ('thematic-break _)
             ('block-quote
              _ ('paragraph _ ('text _ "Foo")))))

(block-expect "parse-blocks, setext heading underline cannot be a lazy continuation line"
  "> foo
bar
==="
  ('document
   _ ('block-quote _ ('paragraph _ ('text _ "foo\nbar\n===")))))

(block-expect "parse-blocks, setext heading underline cannot be a lazy continuation line"
  "- Foo
---"
  ('document _
             ('thematic-break _)
             ('list _ ('item _ ('paragraph _ ('text _ "Foo"))))))

(test-expect-fail 1)
(block-expect "parse-blocks, setext heading a blank line is needed between a paragraph
and a setext heading"
  "Foo
Bar
---"
  ('document _
             ('heading heading-data ('text _ "Foo\nBar")))
  (heading-level heading-data) 2)

(block-expect "parse-blocks, setext heading a blank line is not required before or after
setext headings"
  "---
Foo
---
Bar
---
Baz"
  ('document _
             ('paragraph _ ('text _ "Baz"))
             ('heading heading-data1 ('text _ "Bar"))
             ('heading heading-data2 ('text _ "Foo"))
             ('thematic-break _))
  (heading-level heading-data1) 2
  (heading-level heading-data2) 2)

(block-expect "parse-blocks, setext headings cannot be empty"
  "
===="
  ('document _
             ('paragraph _ ('text _ "===="))))

(block-expect "parse-blocks, setext heading text lines must not be interpretable as block
constructs other than paragraphs"
  "---
---"
  ('document _
             ('thematic-break _)
             ('thematic-break _)))

(block-expect "parse-blocks, setext heading text lines must not be interpretable as block
constructs other than paragraphs"
  "- foo
-----"
  ('document _
             ('thematic-break _)
             ('list _ ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, setext heading text lines must not be interpretable as block
constructs other than paragraphs"
  "    foo
---"
  ('document _
             ('thematic-break _)
             ('code-block _ "foo")))

(block-expect "parse-blocks, setext heading text lines must not be interpretable as block
constructs other than paragraphs"
  "> foo
-----"
  ('document _
             ('thematic-break _)
             ('block-quote _ ('paragraph _ ('text _ "foo")))))

(block-expect "parse-blocks, setext heading use a backslash escape to allow >"
  "\\> foo
------"
  ('document _
             ('heading heading-data ('text _ "\\> foo")))
  (heading-level heading-data) 2)

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
