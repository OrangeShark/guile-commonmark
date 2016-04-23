;; Copyright (C) 2015, 2016  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (commonmark inlines)
  #:use-module (ice-9 regex)
  #:use-module (commonmark node)
  #:export (parse-inlines))

(define re-start-ticks (make-regexp "^`+"))
(define re-ticks (make-regexp "`+"))
(define re-main (make-regexp "^[^`]+"))

(define (start-ticks? text)
  (regexp-exec re-start-ticks (text-value text) (text-position text)))

(define (end-ticks? text)
  (regexp-exec re-ticks (text-value text) (text-position text)))

(define (normal-text? text)
  (regexp-exec re-main (text-value text) (text-position text)))

(define (match-length match)
  (string-length (match:substring match 0)))

(define (make-text text position)
  (cons text position))

(define (text-value text)
  (car text))

(define (text-position text)
  (cdr text))

(define (text-advance text position)
  (make-text (text-value text) position))

(define (text-substring text start end)
  (substring (text-value text) start end))

(define (text-char text)
  (string-ref (text-value text) (text-position text)))

(define (text-end? text)
  (>= (text-position text) (string-length (text-value text))))


;; Node -> Node
;; parses the inline text of paragraphs and heading nodes
(define (parse-inlines node)
  (cond ((not (node? node)) node)
        ((or (paragraph-node? node) (heading-node? node)) (parse-inline node))
        (else (make-node (node-type node) (node-data node) (map parse-inlines (node-children node))))))

(define (parse-ticks text nodes)
  (let ((start-ticks (start-ticks? text)))
    (let loop ((end-ticks (end-ticks? (text-advance text (match:end start-ticks 0)))))
      (cond ((not end-ticks)
             (parse-char (text-advance text (match:end start-ticks 0))
                         (cons (make-text-node (match:substring start-ticks 0)) nodes)))
            ((= (match-length start-ticks) (match-length end-ticks))
             (parse-char (text-advance text (match:end end-ticks 0))
                         (cons (make-code-span-node (text-substring text (match:end start-ticks 0)
                                                                    (match:start end-ticks 0)))
                               nodes)))
            (else (loop (end-ticks? (text-advance text (match:end end-ticks 0)))))))))

(define (parse-normal-text text nodes)
  (let ((normal-text (normal-text? text)))
    (parse-char (text-advance text (match:end normal-text 0))
                (cons (make-text-node (match:substring normal-text 0)) nodes))))

(define (parse-char text nodes)
  (if (text-end? text)
      nodes
      (case (text-char text)
        ((#\`) (parse-ticks text nodes))
        (else (parse-normal-text text nodes)))))

(define (parse-inline node)
  (let ((text (last-child (last-child node))))
    (make-node (node-type node) (node-data node) (parse-char (make-text text 0) '()))))
