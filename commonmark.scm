;; Copyright (C) 2015  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (commonmark)
  #:use-module (commonmark blocks)
  #:use-module (commonmark node)
  #:use-module (commonmark sxml)
  #:export (commonmark->html
            parse-text))


;; Port -> HTML
;; parses a commonmark document and converts it to HTML
(define (commonmark->xml p)
  (document->xml (parse-inlines (parse-blocks p))))


(define (parse-text s)
  (print-node (parse-blocks (open-input-string s))))
