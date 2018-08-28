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

(use-modules (srfi srfi-64)
             (tests utils))

(test-begin "blocks paragraphs")

(block-expect "parse-blocks, simple paragraph"
  "aaa

bbb"
  ('document _
             ('paragraph _ ('text _ "bbb"))
             ('paragraph _ ('text _ "aaa"))))

(block-expect "parse-blocks, paragraph can contain multiple lines, but no blank lines"
  "aaa
bbb

ccc
ddd"
  ('document _
             ('paragraph _ ('text _ "ccc\nddd"))
             ('paragraph _ ('text _ "aaa\nbbb"))))

(block-expect "parse-blocks, multiple blank lines between paragraph have no effect"
  "aaa


bbb"
  ('document _
             ('paragraph _ ('text _ "bbb"))
             ('paragraph _ ('text _ "aaa"))))

(block-expect "parse-blocks, paragraph leading spaces are skipped"
  "  aaa\n bbb"
  ('document _
             ('paragraph _ ('text _ "aaa\nbbb"))))

(block-expect "parse-blocks, paragraph lines after the first may be indented any amount"
  "aaa
             bbb
                                       ccc"
  ('document _
             ('paragraph _ ('text _ "aaa\nbbb\nccc"))))

(block-expect "parse-blocks, paragraph the first line may be indented at most three spaces"
  "   aaa
bbb"
  ('document _
             ('paragraph _ ('text _ "aaa\nbbb"))))

(block-expect "parse-blocks, paragraph the first line may be indented at most three spaces"
  "    aaa
bbb"
  ('document _
             ('paragraph _ ('text _ "bbb"))
             ('code-block _ "aaa")))

(block-expect "parse-blocks, paragraph final spaces are stripped before inline parsing"
  "aaa     
bbb     "
  ('document _
             ('paragraph _ ('text _ "aaa     \nbbb"))))

(test-end)
