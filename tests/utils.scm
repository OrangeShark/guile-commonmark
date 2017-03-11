;; Copyright (C) 2017  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (tests utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-64)
  #:use-module (commonmark blocks)
  #:export (heading-level
            link-references
            info-string
            block-expect))

(define (heading-level heading-data)
  (assq-ref heading-data 'level))

(define (link-references data)
  (assq-ref data 'link-references))

(define (info-string data)
  (assq-ref data 'info-string))

(define-syntax test-asserts
  (syntax-rules ()
    ((_ exp expect-exp)
     (let* ((exp* exp)
            (result (equal? exp* expect-exp)))
       (if result
           result
           (begin
             (format #t "*** Computed: ~s => ~s\n" (quote exp) exp*)
             (format #t "*** Expected: ~s\n" expect-exp)
             #f))))
    ((_ exp expect-exp rest ...)
     (let* ((exp* exp)
            (result (equal? exp* expect-exp)))
       (if result
           (test-asserts rest ...)
           (begin
             (format #t "*** Computed: ~s => ~s\n" (quote exp) exp*)
             (format #t "*** Expected: ~s\n" expect-exp)
             #f))))))

(define-syntax block-expect 
  (syntax-rules ()
    ((_ test-name test-string test-expect)
     (let ((result (call-with-input-string test-string parse-blocks)))
       (test-assert test-name
         (match result
           (test-expect #t)
           (x (format #t "*** Expected: ~s\n" (quote test-expect))
              (format #t "*** Result: ~s\n" x)
              #f)))))
    ((_ test-name test-string test-expect test-assertions ...)
     (let ((result (call-with-input-string test-string parse-blocks)))
       (test-assert test-name
         (match result
           (test-expect (test-asserts test-assertions ...))
           (x (format #t "*** Expected: ~s\n" (quote test-expect))
              (format #t "*** Result: ~s\n" x)
              #f)))))))
