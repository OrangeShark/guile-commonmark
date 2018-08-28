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

(test-begin "blocks block-quotes")

(block-expect "parse-blocks, block quote simple"
  "> # Foo
> bar
> baz"
  ('document _
             ('block-quote _
                           ('paragraph _ ('text _ "bar\nbaz"))
                           ('heading _ ('text _ "Foo")))))

(block-expect "parse-blocks, block quote spaces can be omitted"
  "># Foo
>bar
> baz"
  ('document _
             ('block-quote _
                           ('paragraph _ ('text _ "bar\nbaz"))
                           ('heading _ ('text _ "Foo")))))

(block-expect "parse-blocks, block quote can be indented 1-3 spaces"
  "   > # Foo
   > bar
 > baz"
   ('document _
             ('block-quote _
                           ('paragraph _ ('text _ "bar\nbaz"))
                           ('heading _ ('text _ "Foo")))))

(block-expect "parse-blocks, block quote 4 spaces is a code block"
  "    > # Foo
    > bar
    > baz"
  ('document _
             ('code-block _ "> # Foo\n> bar\n> baz")))

(block-expect "parse-blocks, block quote paragraph laziness"
  "> # Foo
> bar
baz"
  ('document
   _ ('block-quote _
                   ('paragraph _ ('text _ "bar\nbaz"))
                   ('heading _ ('text _ "Foo")))))

(block-expect "parse-blocks, block quote can contain some lazy and some
non-lazy continuation lines"
  "> bar
baz
> foo"
  ('document
   _ ('block-quote
      _ ('paragraph
         _ ('text _ "bar\nbaz\nfoo")))))

(block-expect "parse-blocks, block quote laziness only applies if they would be a paragraph"
  "> foo
---"
  ('document _
             ('thematic-break _)
             ('block-quote _
                           ('paragraph _ ('text _ "foo")))))

(block-expect "parse-blocks, block quote lists are similar"
  "> - foo
- bar"
  ('document _
             ('list
              _ ('item _ ('paragraph _ ('text _ "bar"))))
             ('block-quote
              _ ('list
                 _ ('item
                    _ ('paragraph _ ('text _ "foo")))))))

(block-expect "parse-blocks, block quote code blocks are similar"
  ">     foo
    bar"
  ('document _
             ('code-block _ "bar")
             ('block-quote _ ('code-block _ "foo"))))

(block-expect "parse-blocks, block quote fenced code are similar"
  "> ```
foo
```"
  ('document _
             ('fenced-code _)
             ('paragraph _ ('text _ "foo"))
             ('block-quote _ ('fenced-code _))))

(block-expect "parse-blocks, block quote the following is a lazy continuation line"
  "> foo
    - bar"
  ('document
   _ ('block-quote
      _ ('paragraph _ ('text _ "foo\n- bar")))))

(block-expect "parse-blocks, block quote can be empty"
  ">"
  ('document _ ('block-quote _)))

(block-expect "parse-blocks, block quote can be empty with spaces"
  ">
>  
> "
  ('document _ ('block-quote _)))

(block-expect "parse-blocks, block quote can have initial or final blank lines"
  ">
> foo
>  "
  ('document
   _ ('block-quote
      _ ('paragraph _ ('text _ "foo")))))

(block-expect "parse-blocks, block quote a blank line always separates block quotes"
  "> foo

> bar"
  ('document _
             ('block-quote _ ('paragraph _ ('text _ "bar")))
             ('block-quote _ ('paragraph _ ('text _ "foo")))))

(block-expect "parse-blocks, block quote blank lines in a block quote separates paragraph"
  "> foo
>
> bar"
  ('document
   _ ('block-quote _
                   ('paragraph _ ('text _ "bar"))
                   ('paragraph _ ('text _ "foo")))))

(block-expect "parse-block, block quote can interrupt paragraphs"
  "foo
> bar"
  ('document _
             ('block-quote _ ('paragraph _ ('text _ "bar")))
             ('paragraph _ ('text _ "foo"))))

(block-expect "parse-blocks, block quote blank lines are not needed before or after"
  "> aaa
***
> bbb"
  ('document _
             ('block-quote _ ('paragraph _ ('text _ "bbb")))
             ('thematic-break _)
             ('block-quote _ ('paragraph _ ('text _ "aaa")))))

(block-expect "parse-blocks, block quote blank line needed to break laziness"
  "> bar

baz"
  ('document _
             ('paragraph _ ('text _ "baz"))
             ('block-quote _ ('paragraph _ ('text _ "bar")))))

(block-expect "parse-blocks, block quote blank line needed to break laziness in block quotes as well"
  "> bar
>
baz"
  ('document _
             ('paragraph _ ('text _ "baz"))
             ('block-quote _ ('paragraph _ ('text _ "bar")))))

(block-expect "parse-blocks, block quote laziness rule allowed in nested block quotes"
  "> > > foo
bar"
  ('document
   _ ('block-quote
      _ ('block-quote
         _ ('block-quote
            _ ('paragraph _ ('text _ "foo\nbar")))))))

(block-expect "parse-blocks, block quote laziness rule allowed in nested block quotes of multiple
levels"
  ">>> foo
> bar
>>baz"
  ('document
   _ ('block-quote
      _ ('block-quote
         _ ('block-quote
            _ ('paragraph _ ('text _ "foo\nbar\nbaz")))))))

(block-expect "parse-blocks, block quote code blocks need five spaces"
  ">     code

>    not code"
  ('document _
             ('block-quote _ ('paragraph _ ('text _ "not code")))
             ('block-quote _ ('code-block _ "code"))))

(test-end)
