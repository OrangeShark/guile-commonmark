;; Copyright (C) 2016  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (test-blocks thematic-breaks)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (commonmark blocks))

(test-begin "blocks thematic-breaks")

(test-equal "parse-blocks, thematic breaks with *"
            (call-with-input-string "***\n **  * ** * ** * **" parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks with -"
            (call-with-input-string "---\n - - -" parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks with _"
            (call-with-input-string "___\n   _______________________  " parse-blocks)
            '(document ((closed . #f))
                       (thematic-break ((closed . #t)))
                       (thematic-break ((closed . #t)))))

(test-equal "parse-blocks, thematic breaks must not have other characters"
            (call-with-input-string "---a---" parse-blocks)
            '(document ((closed . #f))
                       (paragraph ((closed . #f))
                                  (text ((closed . #t)) "---a---"))))

(test-assert "parse-blocks, thematic breaks can interupt a paragraph"
             (match (call-with-input-string "Foo\n***\nbar" parse-blocks)
               (('document doc-data
                           ('paragraph para-data1
                                      ('text text-data1 "bar"))
                           ('thematic-break (('closed . #t)))
                           ('paragraph para-data2
                                      ('text text-data2 "Foo")))
                #t)
               (x (pk 'fail x #f))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
