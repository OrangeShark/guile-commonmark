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

(test-begin "blocks lists")

(block-expect "parse-blocks, list changing bullet list delimiter starts a new list"
  "- foo
- bar
+ baz"
  ('document _
             ('list _ ('item _ ('paragraph _ ('text _ "baz"))))
             ('list _
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list changing ordered list delimiter starts a new list"
  "1. foo
2. bar
3) baz"
  ('document _
             ('list list-data ('item _ ('paragraph _ ('text _ "baz"))))
             ('list _
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _ ('paragraph _ ('text _ "foo")))))
  (list-start list-data) 3)

(block-expect "parse-blocks, list can interrupt a paragraph"
  "Foo
- bar
- baz"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "baz")))
                    ('item _ ('paragraph _ ('text _ "bar"))))
             ('paragraph _ ('text _ "Foo"))))


(block-expect "parse-blocks, list must start with 1 to interrupt a paragraph"
  "The number of windows in my house is
14.  The number of doors is 6."
  ('document _
             ('paragraph _ ('text _ "The number of windows in my house is\n14.  The number of doors is 6."))))

(block-expect "parse-blocks, list must start with 1 to interrupt a paragraph"
  "The number of windows in my house is
1.  The number of doors is 6."
  ('document _
             ('list _ ('item _ ('paragraph _ ('text _ "The number of doors is 6."))))
             ('paragraph _ ('text _ "The number of windows in my house is"))))


(block-expect "parse-blocks, list there can be any number of blank lines between items"
  "- foo

- bar


- baz"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "baz")))
                    ('item _ ('paragraph _ ('text _ "bar")))
                    ('item _ ('paragraph _ ('text _ "foo"))))))

(block-expect "parse-blocks, list there can be any number of blank lines between items"
  "- foo
  - bar
    - baz


      bam"
  ('document
   _ ('list
      _ ('list
         _ ('list _
                  ('item _
                         ('paragraph _ ('text _ "bim"))
                         ('paragraph _ ('text _ "baz"))))
         ('paragraph _ ('text _ "bar")))
      ('paragraph _ ('text _ "foo")))))


(block-expect "parse-blocks, separate consecutive lists of the same type with blank HTML comment"
  "- foo
- bar

<!-- -->

- baz
- bim"
  ('document _))

(block-expect "parse-blocks, separate a list from indented code block with blank HTML comment"
  "-   foo

    notcode

-   foo

<!-- -->

    code"
  ('document _))



(block-expect "parse-blocks, list need not be indented to the same level"
  "- a
 - b
  - c
   - d
    - e
   - f
  - g
 - h
- i"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "i")))
                    ('item _ ('paragraph _ ('text _ "h")))
                    ('item _ ('paragraph _ ('text _ "g")))
                    ('item _ ('paragraph _ ('text _ "f")))
                    ('item _ ('paragraph _ ('text _ "e")))
                    ('item _ ('paragraph _ ('text _ "d")))
                    ('item _ ('paragraph _ ('text _ "c")))
                    ('item _ ('paragraph _ ('text _ "b")))
                    ('item _ ('paragraph _ ('text _ "a"))))))

(block-expect "parse-blocks, list need not be indented to the same level"
  "1. a

  2. b

    3. c"
  ('document _
             ('list _
                    ('item _ ('paragraph _ ('text _ "c")))
                    ('item _ ('paragraph _ ('text _ "b")))
                    ('item _ ('paragraph _ ('text _ "a"))))))

(block-expect "parse-blocks, list with a blank line between items is loose"
  "- a
- b

- c"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "c")))
                    ('item _ ('paragraph _ ('text _ "b")))
                    ('item _ ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #f)

(block-expect "parse-blocks, list with a blank line between items is loose"
  "* a
*

* c"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "c")))
                    ('item _)
                    ('item _ ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #f)

(block-expect "parse-blocks, list with a list item with two block level elements is loose"
  "- a
- b

  c
- d"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "d")))
                    ('item _
                           ('paragraph _ ('text _ "c"))
                           ('paragraph _ ('text _ "b")))
                    ('item _ ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #f)

(block-expect "parse-blocks, list with a list item with two block level elements is loose"
  "- a
- b

  [ref]: /url
- d"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "d")))
                    ('item _ ('paragraph _ ('text _ "b")))
                    ('item _ ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #f)

(block-expect "parse-blocks, list with blank lines in code block are tight"
  "- a
- ```
  b


  ```
- c"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "c")))
                    ('item _ ('fenced-code _ "b\n\n"))
                    ('item _ ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #t)

(block-expect "parse-blocks, list is tight even if a sublist is loose"
  "- a
  - b

    c
- d"
  ('document _
             ('list tight-list
                    ('item _ ('paragraph _ ('text _ "d")))
                    ('item _
                           ('list loose-list
                                  ('item _
                                         ('paragraph _ ('text _ "c"))
                                         ('paragraph _ ('text _ "b"))))
                           ('paragraph _ ('text _ "a")))))
  (list-tight? tight-list) #t
  (list-tight? loose-list) #f)

(block-expect "parse-blocks, list is tight because blank line is inside a block quote"
  "* a
  > b
  >
* c"
  ('document _
             ('list list-data 
                    ('item _ ('paragraph _ ('text _ "c")))
                    ('item _
                           ('block-quote _ ('paragraph _ ('text _ "b")))
                           ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #t)

(block-expect "parse-blocks, list is tight because the consecutive block elements are not
separated by blank lines"
  "- a
  > b
  ```
  c
  ```
- d"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "d")))
                    ('item _
                           ('fenced-code _ "c")
                           ('block-quote _ ('paragraph _ ('text _ "b")))
                           ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #t)

(block-expect "parse-blocks, list a single paragraph list is tight"
  "- a"
  ('document _
             ('list list-data
                    ('item _ ('paragraph _ ('text _ "a")))))
  (list-tight? list-data) #t)

(block-expect "parse-blocks, list a single paragraph is tight"
  "- a
  - b"
  ('document _
             ('list list-data1
                    ('item _
                           ('list list-data2
                                  ('item _ ('paragraph _ ('text _ "b"))))
                           ('paragraph _ ('text _ "a")))))
  (list-tight? list-data1) #t
  (list-tight? list-data2) #t)

(block-expect "parse-blocks, list is loose because the blank line between two block elements"
  "1. ```
   foo
   ```

   bar"
  ('document _
             ('list list-data
                    ('item _
                           ('paragraph _ ('text _ "bar"))
                           ('fenced-code _ "foo"))))
  (list-tight? list-data) #f)

(block-expect "parse-blocks, outer list is loose and inner list tight"
  "* foo
  * bar

  baz"
  ('document _
             ('list outer-list
                    ('item _
                           ('paragraph _ ('text _ "baz"))
                           ('list inner-list
                                  ('item _ ('paragraph _ ('text _ "bar"))))
                           ('paragraph _ ('text _ "foo")))))
  (list-tight? outer-list) #f
  (list-tight? inner-list) #t)



(block-expect "parse-blocks, outer list is loose and inner list tight"
  "- a
  - b
  - c

- d
  - e
  - f"
  ('document _
             ('list outer-list
                    ('item _
                           ('list inner-list1
                                  ('item _ ('paragraph _ ('text _ "f")))
                                  ('item _ ('paragraph _ ('text _ "e"))))
                           ('paragraph _ ('text _ "d")))
                    ('item _
                           ('list inner-list2
                                  ('item _ ('paragraph _ ('text _ "c")))
                                  ('item _ ('paragraph _ ('text _ "b"))))
                           ('paragraph _ ('text _ "a")))))
  (list-tight? outer-list) #f
  (list-tight? inner-list1) #t
  (list-tight? inner-list2) #t)

(test-end)

