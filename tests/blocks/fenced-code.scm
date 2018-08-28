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
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (commonmark blocks)
             (tests utils))

(test-begin "blocks fenced-code")

(block-expect "parse-blocks, fenced code simple"
  "```
<
 >
```"
  ('document _
             ('fenced-code _ "<\n >")))

(block-expect "parse-blocks, fenced code simple with tildes"
  "~~~
<
 >
~~~"
  ('document _
             ('fenced-code _ "<\n >")))

(block-expect "parse-blocks, fenced code must use the same character as the opening"
  "```
aaa
~~~
```"
  ('document _
             ('fenced-code _ "aaa\n~~~")))


(block-expect "parse-blocks, fenced code must use the same character as the opening"
  "~~~
aaa
```
~~~"
  ('document _
             ('fenced-code _ "aaa\n```")))

(block-expect "parse-blocks, fenced code closing fence must be at least as long as
the opening fence"
  "````
aaa
```
``````"
  ('document _
             ('fenced-code _ "aaa\n```")))

(block-expect "parse-blocks, fenced code closing fence must be at least as long as
the opening fence"
  "~~~~
aaa
~~~
~~~~"
  ('document _
             ('fenced-code _ "aaa\n~~~")))

(block-expect "parse-blocks, fenced code unclosed code blocks are closed by the end of
the document"
  "```"
  ('document _
             ('fenced-code _)))

(block-expect "parse-blocks fenced code unclosed code blocks are closed by the end of
the document"
  "`````

```
aaa"
  ('document _
             ('fenced-code _ "\n```\naaa")))

(block-expect "parse-blocks, fenced code unclosed code blocks are closed by the end of
block quote"
  "> ```
> aaa

bbb"
  ('document _
             ('paragraph _ ('text _ "bbb"))
             ('block-quote _
                           ('fenced-code _ "aaa"))))

(block-expect "parse-blocks, fenced code can have all empty lines as its content"
  "```

  
```"
  ('document _
             ('fenced-code _ "\n  ")))

(block-expect "parse-blocks, fenced code can be empty"
  "```
```"
  ('document _
             ('fenced-code _)))

(block-expect "parse-blocks, fenced code can be indented with equivalent opening
indentation removed"
  " ```
 aaa
aaa
```"
  ('document _
             ('fenced-code _ "aaa\naaa")))

(block-expect "parse-blocks, fenced code can be indented with equivalent opening
indentation removed"
  "  ```
aaa
  aaa
aaa
  ```"
  ('document _
             ('fenced-code _ "aaa\naaa\naaa")))

(block-expect "parse-blocks, fenced code can be indented with equivalent opening
indentation removed"
  "   ```
   aaa
    aaa
  aaa
   ```"
  ('document _
             ('fenced-code _ "aaa\n aaa\naaa")))

(block-expect "parse-blocks, fenced code four spaces indentation produces code-block"
  "    ```
    aaa
    ```"
  ('document _
             ('code-block _ "```\naaa\n```")))

(block-expect "parse-blocks, fenced code closing fence may be indented by 0-3 spaces
and does not need to match opening fence indentation"
  "```
aaa
  ```"
  ('document _
             ('fenced-code _ "aaa")))

(block-expect "parse-blocks, fenced code closing fence may be indented by 0-3 spaces
and does not need to match opening fence indentation"
  "   ```
aaa
  ```"
  ('document _
             ('fenced-code _ "aaa")))

(block-expect "parse-blocks, fenced code not a closing fence because it is indented 4 spaces"
  "```
aaa
    ```"
  ('document _
             ('fenced-code _ "aaa\n    ```")))

(block-expect "parse-blocks, fenced code fences cannot contain internal spaces"
  "``` ```
aaa"
  ('document _
             ('paragraph _ ('text _ "``` ```\naaa"))))

(block-expect "parse-blocks, fenced code fences cannot contain internal spaces"
  "~~~~~~
aaa
~~~ ~~~"
  ('document _
             ('fenced-code _ "aaa\n~~~ ~~~")))

(block-expect "parse-blocks, fenced code can interrupt paragraphs, and can be followed
by paragraphs, without a blank line between"
  "foo
```
bar
```
baz"
  ('document _
             ('paragraph _ ('text _ "baz"))
             ('fenced-code _ "bar")
             ('paragraph _ ('text _ "foo"))))

(block-expect "parse-blocks, fenced code, other blocks can occur before and after fenced code
blocks without a blank line"
  "foo
---
~~~
bar
~~~
# baz"
  ('document _
             ('heading _ ('text _ "baz"))
             ('fenced-code _ "bar")
             ('heading _ ('text _ "foo"))))

(block-expect "parse-blocks, fenced code info string"
  "```ruby
def foo(x)
  return 3
end
```"
  ('document _
             ('fenced-code code-data "def foo(x)\n  return 3\nend"))
  (info-string code-data) "ruby")

(block-expect "parse-blocks, fenced code info strings backtick code blocks cannot
contain backticks"
  "``` aa ```
foo"
  ('document _
             ('paragraph _ ('text _ "``` aa ```\nfoo"))))

(block-expect "parse-blocks, fenced code closing fences cannot have info strings"
  "```
``` aaa
```"
  ('document _
             ('fenced-code _ "``` aaa")))

(test-end)
