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

(test-begin "blocks list-items")

(block-expect "parse-blocks, list item basic case"
  "1.  A paragraph
    with two lines.

        indented code

    > A block quote."
  ('document _
             ('list _
                    ('item _
                           ('block-quote _
                                         ('paragraph _ ('text _ "A block quote.")))
                           ('code-block _ "indented code")
                           ('paragraph _ ('text _ "A paragraph\nwith two lines."))))))

(block-expect "parse-blocks, list item content must be indented enough to be part of the item"
  "- one

 two"
  ('document _
             ('paragraph _ ('text _ "two"))
             ('list _ ('item _ ('paragraph _ ('text _ "one"))))))

(block-expect "parse-blocks, list item content must be indented enough to be part of the item"
  "- one

  two"
  ('document _
             ('list _
                    ('item _
                           ('paragraph _ ('text _ "two"))
                           ('paragraph _ ('text _ "one"))))))

(block-expect "parse-blocks, list item content must be indented enough to be part of the item"
  " -    one

     two"
  ('document _
             ('code-block _ " two")
             ('list _ ('item _ ('paragraph _ ('text _ "one"))))))

(block-expect "parse-blocks, list item content must be indented enough to be part of the item"
  " -    one

      two"
  ('document _
             ('list _
                    ('item _
                           ('paragraph _ ('text _ "two"))
                           ('paragraph _ ('text _ "one"))))))

(block-expect "parse-blocks, list item content must be indented enough to be part of the item
not column"
  "   > > 1.  one
>>
>>     two"
  ('document
   _ ('block-quote
      _ ('block-quote
         _ ('list
            _ ('item _
                     ('paragraph _ ('text _ "two"))
                     ('paragraph _ ('text _ "one"))))))))

(block-expect "parse-blocks, list item content must be indented enough to be part of the item
not column"
  ">>- one
>>
  >  > two"
  ('document
   _ ('block-quote
      _ ('block-quote _
                      ('paragraph _ ('text _ "two"))
                      ('list _ ('item _ ('paragraph _ ('text _ "one"))))))))

(block-expect "parse-blocks, list item needs one space after the list marker"
  "-one

2.two"
  ('document _
             ('paragraph _ ('text _ "2.two"))
             ('paragraph _ ('text _ "-one"))))

(block-expect "parse-blocks, list item may contain blocks separated by more than one blank
line"
  "- foo


  bar"
  ('document _
             ('list _
                    ('item _
                           ('paragraph _ ('text _ "bar"))
                           ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item may contain any kind of block"
  "1.  foo

    ```
    bar
    ```

    baz

    > bam"
  ('document _
             ('list _
                    ('item _
                           ('block-quote _ ('paragraph _ ('text _ "bam")))
                           ('paragraph _ ('text _ "baz"))
                           ('fenced-code _ "bar")
                           ('paragraph _ ('text _ "foo"))))))


(block-expect "parse-blocks, list item contains indented code block preserve empty lines"
  "- Foo

      bar


      baz"
  ('document _
             ('list _
                    ('item _
                           ('code-block _ "bar\n\n\nbaz")
                           ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item start numbers must be nine digits or less"
  "123456789. ok"
  ('document _
             ('list list-data 
                    ('item _ ('paragraph _ ('text _ "ok")))))
  (list-start list-data) 123456789)

(block-expect "parse-blocks, list item start numbers must be nine digits or less"
  "1234567890. not ok"
  ('document _
             ('paragraph _ ('text _ "1234567890. not ok"))))

(block-expect "parse-blocks, list item may begin with 0s"
  "0. ok"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "ok")))))
  (list-start list-data) 0)

(block-expect "parse-blocks, list item may begin with 0s"
  "003. ok"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "ok")))))
  (list-start list-data) 3)

(block-expect "parse-blocks, list item number may not be negative"
  "-1. not ok"
  ('document _ ('paragraph _ ('text _ "-1. not ok"))))

(block-expect "parse-blocks, list item code block must be indented four spaces beyond the edge"
  "- foo

      bar"
  ('document _
             ('list _
                    ('item _
                           ('code-block _ "bar")
                           ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item code block must be indented four spaces beyond the edge"
  "  10.  foo

           bar"
  ('document _
             ('list _
                    ('item _
                           ('code-block _ "bar")
                           ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item if code block is first block, contents must be indented
one space after"
  "1.     indented code

   paragraph

       more code"
  ('document _
             ('list _
                    ('item _
                           ('code-block _ "more code")
                           ('paragraph _ ('text _ "paragraph"))
                           ('code-block _ "indented code")))))

(block-expect "parse-blocks, list item if code block is first block, additional space inside
code block"
  "1.      indented code

   paragraph

       more code"
  ('document _
             ('list _
                    ('item _
                           ('code-block _ "more code")
                           ('paragraph _ ('text _ "paragraph"))
                           ('code-block _ " indented code")))))

(block-expect "parse-blocks, list item does not not apply with three-space indent"
  "-    foo

  bar"
  ('document _
             ('paragraph _ ('text _ "bar"))
             ('list _ ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item can start with a blank line"
  "-
  foo
-
  ```
  bar
  ```
-
      baz"
  ('document _
             ('list _
                    ('item _ ('code-block _ "baz"))
                    ('item _ ('fenced-code _ "bar"))
                    ('item _ ('paragraph _ ('text _ "foo"))))))


(block-expect "parse-blocks, list item blank line, the number of spaces does not change
required indentation"
  "-   
  foo"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item can begin with at most one blank line"
  "-

  foo"
  ('document _
             ('paragraph _ ('text _ "foo"))
             ('list _ ('item _))))

(block-expect "parse-blocks, list item empty bullet list item"
  "- foo
-
- bar"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _)
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item empty bullet list item space does not matter after list marker"
  "- foo
-   
- bar"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _)
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item empty ordered list item"
  "1. foo
2.
3. bar"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _)
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, a list may start or end with an empty list item"
  "*"
  ('document _
             ('list _
                    ('item _))))


(block-expect "parse-blocks, an empty list item cannot interrupt a paragraph"
  "foo
*

foo
1."
  ('document _
             ('paragraph _ ('text "foo\n1."))
             ('paragraph _ ('text "foo\n*"))))

(block-expect "parse-blocks, list item indented one space"
  " 1.  A paragraph
     with two lines.

         indented code

     > A block quote."
 ('document _
            ('list _
                   ('item _
                          ('block-quote _ ('paragraph _ ('text _ "A block quote.")))
                          ('code-block _  "indented code")
                          ('paragraph _ ('text _ "A paragraph\nwith two lines."))))))

(block-expect "parse-blocks, list item indented two spaces"
  "  1.  A paragraph
      with two lines.

          indented code

      > A block quote."
 ('document _
            ('list _
                   ('item _
                          ('block-quote _ ('paragraph _ ('text _ "A block quote.")))
                          ('code-block _  "indented code")
                          ('paragraph _ ('text _ "A paragraph\nwith two lines."))))))

(block-expect "parse-blocks, list item indented three spaces"
  "   1.  A paragraph
       with two lines.

           indented code

       > A block quote."
 ('document _
            ('list _
                   ('item _
                          ('block-quote _ ('paragraph _ ('text _ "A block quote.")))
                          ('code-block _  "indented code")
                          ('paragraph _ ('text _ "A paragraph\nwith two lines."))))))

(block-expect "parse-blocks, list item indented four spaces gives a code block"
  "    1.  A paragraph
        with two lines.

            indented code

        > A block quote."
 ('document _
            ('code-block _  "1.  A paragraph
    with two lines.

        indented code

    > A block quote.")))

(block-expect "parse-blocks, list item lazy continuation lines"
  "  1.  A paragraph
with two lines.

          indented code

      > A block quote."
  ('document _
             ('list _
                    ('item _
                           ('block-quote _ ('paragraph _ ('text _ "A block quote.")))
                           ('code-block _  "indented code")
                           ('paragraph _ ('text _ "A paragraph\nwith two lines."))))))


(block-expect "parse-blocks, list item identation can be partially deleted"
  "  1.  A paragraph
    with two lines."
  ('document _
             ('list _
                    ('item _
                           ('paragraph _ ('text _ "A paragraph\nwith two lines."))))))

(block-expect "parse-blocks, list item lazy continuation in nested structures"
  "> 1. > Blockquote
continued here."
  ('document
   _ ('block-quote
      _ ('list
         _ ('item _ ('block-quote _ ('paragraph _ ('text _ "Blockquote\ncontinued here."))))))))

(block-expect "parse-blocks, list item lazy continuation in nested structures"
  "> 1. > Blockquote
> continued here."
  ('document
   _ ('block-quote
      _ ('list
         _ ('item _ ('block-quote _ ('paragraph _ ('text _ "Blockquote\ncontinued here."))))))))

(block-expect "parse-blocks, list item with sublists need to be indented"
  "- foo
  - bar
    - baz
      - boo"
  ('document
   _ ('list
      _ ('item
         _ ('list
            _ ('item
               _ ('list
                  _ ('item
                     _ ('list
                        _ ('item _ ('paragraph _ ('text _ "boo"))))
                     ('paragraph _ ('text _ "baz"))))
               ('paragraph _ ('text _ "bar"))))
         ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item with sublists need to be indented, one is not enough"
  "- foo
 - bar
  - baz
   - boo"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "boo")))
                    ('item _ ('paragraph _ ('text _ "baz")))
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item with sublists need to be indented, need four here"
  "10) foo
    - bar"
  ('document _
             ('list _
                    ('item _
                           ('list _ ('item _ ('paragraph _ ('text _ "bar"))))
                           ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item with sublists need to be indented, three not enough here"
  "10) foo
   - bar"
  ('document _
             ('list _ ('item _ ('paragraph _ ('text _ "bar"))))
             ('list _ ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list item may have a list as the first block"
  "- - foo"
  ('document _
             ('list _ ('item _ ('list _ ('item _ ('paragraph _ ('text _ "foo"))))))))

(block-expect "parse-blocks, list item may have a list as the first block"
  "1. - 2. foo"
  ('document
   _ ('list
      _ ('item
         _ ('list
            _ ('item
               _ ('list
                  _ ('item _ ('paragraph _ ('text _ "foo"))))))))))

(block-expect "parse-blocks, list item can contain a heading"
  "- # Foo
- Bar
  ---
  baz"
  ('document _
             ('list _
                    ('item _
                           ('paragraph _ ('text _ "baz"))
                           ('heading _ ('text _ "Bar")))
                    ('item _ ('heading _ ('text _ "Foo"))))))

(test-end)

