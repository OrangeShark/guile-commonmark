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

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-64)
             (ice-9 match)
             (tests utils)
             (commonmark blocks))

(test-begin "blocks atx-headings")

(block-expect "parse-blocks, atx headings"
  "# foo
## foo
### foo
#### foo
##### foo
###### foo"
  ('document _
             ('heading heading-data6
                       ('text _ "foo"))
             ('heading heading-data5
                       ('text _ "foo"))
             ('heading heading-data4
                       ('text _ "foo"))
             ('heading heading-data3
                       ('text _ "foo"))
             ('heading heading-data2
                       ('text _ "foo"))
             ('heading heading-data1
                       ('text _ "foo")))
  (heading-level heading-data6) 6
  (heading-level heading-data5) 5
  (heading-level heading-data4) 4
  (heading-level heading-data3) 3
  (heading-level heading-data2) 2
  (heading-level heading-data1) 1)

(block-expect "parse-blocks, atx headings not more than 6 #"
  "####### foo"
  ('document _
             ('paragraph _
                         ('text _ "####### foo"))))

(block-expect "parse-blocks, atx headings requires an empty space"
  "#5 bolt

#hashtag"
  ('document _ 
             ('paragraph _
                         ('text _ "#hashtag"))
             ('paragraph _
                         ('text _ "#5 bolt"))))

(block-expect "parse-blocks, atx headings not a heading when # is escaped"
  "\\## foo"
  ('document _
             ('paragraph _
                         ('text _ "\\## foo"))))

(block-expect "parse-blocks, atx headings leading and trailing blanks are ignored"
  "#                  foo                  "
  ('document _ 
             ('heading heading-data
                       ('text _ "foo")))
  (heading-level heading-data) 1)

(block-expect "parse-blocks, atx headings one to three spaces indentation are allowed"
  " ### foo
  ## foo
   # foo"
  ('document _
             ('heading heading-data3
                       ('text _ "foo"))
             ('heading heading-data2
                       ('text _ "foo"))
             ('heading heading-data1
                       ('text _ "foo")))
  (heading-level heading-data3) 1 
  (heading-level heading-data2) 2
  (heading-level heading-data1) 3)

(block-expect "parse-blocks, atx heading four spaces are too much"
  "    # foo"
  ('document _
             ('code-block _ "# foo")))

(block-expect "parse-blocks, atx heading four spaces are too much"
  "foo
    # bar"
  ('document _
             ('paragraph _ ('text _ "foo\n# bar"))))

(block-expect "parse-blocks, atx headings closing # characters are optional"
  "## foo ##
  ###   bar    ###"
  ('document _ 
             ('heading heading-data1
                       ('text _ "bar"))
             ('heading heading-data2
                       ('text _ "foo")))
  (heading-level heading-data1) 3
  (heading-level heading-data2) 2)

(block-expect "parse-blocks, atx headings closing sequence does not need be same length"
  "# foo ##################################
##### bar ##"
  ('document _
             ('heading heading-data1
                       ('text _ "bar"))
             ('heading heading-data2
                       ('text _ "foo")))
  (heading-level heading-data1) 5 
  (heading-level heading-data2) 1)

(block-expect "parse-blocks, atx headings spaces are allowed after closing sequence"
  "### foo ###     "
  ('document _
             ('heading heading-data
                         ('text _ "foo")))
  (heading-level heading-data) 3)

(block-expect "parse-blocks, atx headings nonspace character after closing sequence"
  "### foo ### b"
  ('document _
             ('heading heading-data
                       ('text _ "foo ### b")))
  (heading-level heading-data) 3)

(block-expect "parse-blocks, atx headings closing sequence must be preceded by a space"
  "# foo#"
  ('document _
             ('heading heading-data
                       ('text _ "foo#")))
  (heading-level heading-data) 1)

(block-expect "parse-blocks, atx headings need not be separated from surrounding content
by blank lines"
  "****
## foo
****"
  ('document _
             ('thematic-break _)
             ('heading heading-data
                       ('text _ "foo"))
             ('thematic-break _))
  (heading-level heading-data) 2)

(block-expect "parse-blocks, atx headings can interrupt paragraphs"
  "Foo bar
# baz
Bar foo"
  ('document _
             ('paragraph _
                         ('text _ "Bar foo"))
             ('heading heading-data
                       ('text _ "baz"))
             ('paragraph _
                         ('text _ "Foo bar")))
  (heading-level heading-data) 1)

(block-expect "parse-blocks, atx headings can be empty"
  "## \n#\n### ###"
  ('document _
             ('heading heading-data1
                       ('text _ ""))
             ('heading heading-data2
                       ('text _ ""))
             ('heading heading-data3
                       ('text _ "")))
  (heading-level heading-data1) 3
  (heading-level heading-data2) 1
  (heading-level heading-data3) 2)


(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
