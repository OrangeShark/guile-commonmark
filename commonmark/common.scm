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

(define-module (commonmark common) 
  #:export (ascii-punctuation-characters
            escaped-characters
            regular-characters
            in-parens-no-space
            link-destination
            link-title
            remove-quotes))

;; ']' needs to be the first character after an openning '[' to be able
;; to match ']'
(define ascii-punctuation-characters "]!\"#$%&'()*+,-./:;<=>?@[\\^_`{|}~")
(define escaped-characters (string-append "\\\\[" ascii-punctuation-characters "]"))
(define regular-characters "[^\x01-\x19 ()\\\\]")
(define in-parens-no-space (string-append "\\((" regular-characters "|" escaped-characters "|\\\\)*\\)"))
(define link-destination (string-append "((" regular-characters "+|"
                                        escaped-characters "|"
                                        "[\\][^() \t\v\f\r\n]|"
                                        in-parens-no-space ")+)"))
(define link-title (string-append "((\"(" escaped-characters "|[^\"])*\"|"
                                  "'(" escaped-characters "|[^'])*'))"))


(define (remove-quotes str)
  (substring str 1 (- (string-length str) 1)))
