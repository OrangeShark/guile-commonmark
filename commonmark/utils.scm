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

(define-module (commonmark utils)
  #:use-module (ice-9 rdelim)
  #:export (map&co
            expand-tabs
            read-tabless-line))


;; f: (a k' -> d)
;; k': b (listof c) -> d
;; l: (listof a)
;; k: (listof b) (listof c) -> d
;; f l k -> d
(define (map&co f l k)
  "like map but uses a continuation to collect an extra list of values"
  (if (null? l)
      (k '() '())
      (f (car l) (lambda (v d)
                   (map&co f (cdr l)
                           (lambda (v2 d2)
                             (k (cons v v2) (append d d2))))))))


;; String -> String
(define (expand-tabs line)
  "Expands the tabs of the string line into spaces"
  (list->string
   (let loop ((l (string->list line)) (c 1))
     (cond [(null? l) '()]
           [(char=? #\tab (car l))
            (if (= (remainder c 4) 0)
                (cons #\space
                      (loop (cdr l) (1+ c)))
                (cons #\space
                      (loop l (1+ c))))]
           [else (cons (car l)
                       (loop (cdr l) (1+ c)))]))))

;; Line is one of:
;;  - String
;;  - eof-object

;; Port -> Line
(define (read-tabless-line p)
  "read a line from port p and expands any tabs"
  (let ((line (read-line p)))
    (if (eof-object? line)
        line
        (expand-tabs line))))
