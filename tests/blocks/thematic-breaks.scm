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

(test-begin "blocks thematic-breaks")

(block-expect "parse-blocks, basic thematic breaks"
  "***
---
___"
  ('document _
             ('thematic-break _)
             ('thematic-break _)
             ('thematic-break _)))

(block-expect "parse-blocks, not enough characters"
  "--
**
__"
  ('document _
             ('paragraph _
                         ('text _ "--\n**\n__"))))

(block-expect "parse-blocks, thematic break one to three spaces indent are allowed"
  " ***
  ***
   ***"
  ('document _
             ('thematic-break _)
             ('thematic-break _)
             ('thematic-break _)))

(block-expect "parse-blocks, thematic break four spaces is too many"
  "    ***"
  ('document _
             ('code-block _ "***")))

(block-expect "parse-blocks, thematic break four spaces is too many"
  "Foo
    ***"
  ('document _
             ('paragraph _
                         ('text _ "Foo\n***"))))

(block-expect "parse-blocks, thematic break more than three characters may be used"
  "_____________________________________"
  ('document _
             ('thematic-break _)))

(block-expect "parse-blocks, thematic break spaces are allowed between the characters"
  " - - -"
  ('document _
             ('thematic-break _)))

(block-expect "parse-blocks, thematic break spaces are allowed between the characters"
  " **  * ** * ** * **"
  ('document _
             ('thematic-break _)))

(block-expect "parse-blocks, thematic break spaces are allowed between the characters"
  "-     -      -      -"
  ('document _
             ('thematic-break _)))

(block-expect "parse-blocks, thematic break spaces are allowed at the end"
  "- - - -    "
  ('document _
             ('thematic-break _)))

(block-expect "parse-blocks, thematic break no other characters may occur in the line"
  "_ _ _ _ a

a------

---a---"
  ('document _
             ('paragraph _ ('text _ "---a---"))
             ('paragraph _ ('text _ "a------"))
             ('paragraph _ ('text _ "_ _ _ _ a"))))

(block-expect "parse-blocks, thematic break all characters need to be the same"
  " *-*"
  ('document _
             ('paragraph _ ('text _ "*-*"))))

(block-expect "parse-blocks, thematic break do not need blank lines before or after"
  "- foo
***
- bar"
  ('document _
             ('list _ ('item _ ('paragraph _ ('text _ "bar"))))
             ('thematic-break _)
             ('list _ ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, thematic break can interrupt a paragraph"
  "Foo
***
bar"
  ('document _
             ('paragraph _ ('text _ "bar"))
             ('thematic-break _)
             ('paragraph _ ('text _ "Foo"))))

(block-expect "parse-blocks, thematic break line of dashes interpreted as setext heading"
  "Foo
---
bar"
  ('document _
             ('paragraph _ ('text _ "bar"))
             ('heading _ ('text _ "Foo"))))

(block-expect "parse-blocks, thematic break takes precedence with lists"
  "* Foo
* * *
* Bar"
  ('document _
             ('list _ ('item _ ('paragraph _ ('text _ "Bar"))))
             ('thematic-break _)
             ('list _ ('item _ ('paragraph _ ('text _ "Foo"))))))

(block-expect "parse-blocks, thematic break use a different symbol in a list"
  "- Foo
- * * *"
  ('document _
             ('list _
                    ('item _ ('thematic-break _))
                    ('item _ ('paragraph _ ('text _ "Foo"))))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
