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

(define-module (commonmark node)
  #:use-module (srfi srfi-9)
  #:export (make-node
            node-type
            node-children
            node-data
            node-closed?
            no-children?
            document-node?
            hrule-node?
            paragraph-node?
            block-quote-node?
            code-block-node?
            fenced-code-node?
            list-node?
            item-node?
            header-node?
            text-node?
            softbreak-node?
            child-closed?
            close-node
            last-child
            rest-children
            print-node))

;; Node-Type is one of:
;; - 'document
;; - 'hrule
;; - 'paragraph
;; - 'block-quote
;; - 'code-block
;; - 'fenced-code
;; - 'list
;; - 'item
;; - 'header
;; - 'text
;; - 'softbreak
;; interp. The type of CommonMark block node

(define-record-type <node>
  (make-node type children data closed)
  node?
  (type node-type)
  (children node-children)
  (data node-data)
  (closed node-closed?))
;; Node is (make-node Node-Type (listof Node) Node-Data Boolean)
;; interp. a node to represent a CommonMark document

;; (listof Node) -> Boolean
;; returns true if the n has no children
(define (no-children? n)
  (null? (node-children n)))

;; Node Node-Type -> Boolean
(define (node-type? n t)
  (eq? (node-type n) t))

;; Node -> Boolean
(define (document-node? n)
  (node-type? n 'document))

(define (hrule-node? n)
  (node-type? n 'hrule))

(define (paragraph-node? n)
  (node-type? n 'paragraph))

(define (block-quote-node? n)
  (node-type? n 'block-quote))

(define (code-block-node? n)
  (node-type? n 'code-block))

(define (fenced-code-node? n)
  (node-type? n 'fenced-code))

(define (list-node? n)
  (node-type? n 'list))

(define (item-node? n)
  (node-type? n 'item))

(define (header-node? n)
  (node-type? n 'header))

(define (text-node? n)
  (node-type? n 'text))

(define (softbreak-node? n)
  (node-type? n 'softbreak))

(define (child-closed? n)
  (node-closed? (last-child n)))



;; Node -> Node
;; closes the node without changing any of the other properties
(define (close-node n)
  (make-node (node-type n) (node-children n) (node-data n) #t))

(define (last-child n)
  (car (node-children n)))

(define (rest-children n)
  (cdr (node-children n)))

(define (print-node n)
  (define (inner n d)
    (cond ((null? n) #f)
          ((string? n)
           (display "\"")
           (display n)
           (display "\""))
          ((equal? (node-type n) 'text) 
           (add-depth d) 
           (display (node-children n))) 
          (else
           (add-depth d)
           (display (node-type n))
           (map (lambda (n) 
                  (newline)
                  (inner n (+ d 1))) (node-children n)))))
  (inner n 0)
  (newline))

(define (add-depth d)
  (when (> d 0)
    (display "   ")
    (add-depth (- d 1))))
