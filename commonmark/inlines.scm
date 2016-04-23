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

(define (start-ticks? text position)
  (regexp-exec re-start-ticks text position))

(define (end-ticks? text position)
  (regexp-exec re-ticks text position))

(define (normal-text? text position)
  (regexp-exec re-main text position))

(define (match-length match)
  (string-length (match:substring match 0)))
;; Node -> Node
;; parses the inline text of paragraphs and heading nodes
(define (parse-inlines node)
  (cond ((not (node? node)) node)
        ((or (paragraph-node? node) (heading-node? node)) (parse-inline node))
        (else (make-node (node-type node) (node-data node) (map parse-inlines (node-children node))))))

(define (parse-ticks text position nodes)
  (let ((start-ticks (start-ticks? text position)))
    (let loop ((end-ticks (end-ticks? text (match:end start-ticks 0))))
      (cond ((not end-ticks)
             (parse-char text (match:end start-ticks 0)
                         (cons (make-text-node (match:substring start-ticks 0)) nodes)))
            ((= (match-length start-ticks) (match-length end-ticks))
             (parse-char text (match:end end-ticks 0)
                         (cons (make-code-span-node (substring text (match:end start-ticks 0)
                                                               (match:start end-ticks 0)))
                               nodes)))
            (else (loop (end-ticks? text (match:end end-ticks 0))))))))

(define (parse-normal-text text position nodes)
  (let ((normal-text (normal-text? text position)))
    (parse-char text (match:end normal-text 0)
                (cons (make-text-node (match:substring normal-text 0)) nodes))))

(define (parse-char text position nodes)
  (if (>= position (string-length text))
      nodes
      (case (string-ref text position)
        ((#\`) (parse-ticks text position nodes))
        (else (parse-normal-text text position nodes)))))

(define (parse-inline node)
  (let ((text (last-child (last-child node))))
    (make-node (node-type node) (node-data node) (parse-char text 0 '()))))
