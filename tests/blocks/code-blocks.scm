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

(test-begin "blocks code-blocks")

(block-expect "parse-blocks, indented code block"
  "    a simple
      indented code block"
  ('document _ ('code-block _ "a simple\n  indented code block")))

(block-expect "parse-blocks, indented code block, list takes precedence"
  "  - foo

    bar"
  ('document
   _ ('list
      _ ('item _
               ('paragraph _ ('text _ "bar"))
               ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, indented code block, list takes precedence"
  "1.  foo
    - bar"
  ('document
   _ ('list _
            ('item _
                   ('list _ ('item _ ('paragraph _ ('text _ "bar"))))
                   ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, indented code block chunks separated by blank lines"
  "    chunk1

    chunk2
  
 
 
    chunk3"
  ('document _
             ('code-block _ "chunk1\n\nchunk2\n\n\n\nchunk3")))

(block-expect "parse-blocks, indented code block spaces beyond four are included even
blank lines"
  "    chunk1
      
      chunk2"
  ('document _
             ('code-block _ "chunk1\n  \n  chunk2")))

(block-expect "parse-blocks, indented code block cannot interrupt a paragraph"
  "Foo
    bar"
  ('document _ ('paragraph _ ('text _ "Foo\nbar"))))

(block-expect "parse-blocks, indented code block any non-blank line with fewer than four
leading spaces ends code block immediately"
  "    foo
bar"
  ('document _
             ('paragraph _ ('text _ "bar"))
             ('code-block _ "foo")))

(block-expect "parse-blocks, indented code block can occur immediately before and after
other kinds of blocks"
  "# Heading
    foo
Heading
------
    foo
----"
  ('document _
             ('thematic-break _)
             ('code-block _ "foo")
             ('heading _ ('text _ "Heading"))
             ('code-block _ "foo")
             ('heading _ ('text _ "Heading"))))

(block-expect "parse-blocks, indented code block, first line can be indented more than
four spaces"
  "        foo
    bar"
  ('document _ ('code-block _ "    foo\nbar")))

(block-expect "parse-blocks, indented code block, blank lines preceding
or following an indented code block are not included"
  "
    
    foo
    "
  ('document _ ('code-block _ "foo")))

(block-expect "parse-blocks, indented code block trailing spaces are included"
  "    foo  "
  ('document _ ('code-block _ "foo  ")))

(test-end)
