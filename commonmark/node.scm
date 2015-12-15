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
            make-document-node
            document-node?
            make-hrule-node
            hrule-node?
            make-paragraph-node
            paragraph-node?
            make-block-quote-node
            block-quote-node?
            make-code-block-node
            code-block-node?
            make-fenced-code-node
            fenced-code-node?
            make-list-node
            list-node?
            make-item-node
            item-node?
            make-header-node
            header-node?
            make-text-node
            text-node?
            make-softbreak-node
            softbreak-node?
            child-closed?
            close-node
            last-child
            rest-children
            add-child-node
            replace-last-child
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


;; Document node
;; A document node is the root of a commonmark document
(define (make-document-node)
  (make-node 'document '() '() #f))

;; Node -> Boolean
(define (document-node? n)
  (node-type? n 'document))

;; Hrule node
;; A hrule node represents a horizontal rule in a commonmark document
(define (make-hrule-node)
  (make-node 'hrule '() '() #t))

;; Node -> Boolean
(define (hrule-node? n)
  (node-type? n 'hrule))

;; Paragraph node
;; A paragraph node represents a paragraph in a commonmark document
;; with text nodes as children
;; String -> Node
(define (make-paragraph-node text)
  (make-node 'paragraph (list (make-text-node text)) '() #f))

;; Node -> Boolean
(define (paragraph-node? n)
  (node-type? n 'paragraph))

;; Block quote node
;; A block quote node represents a block quote in a commonmark document
;; which contains other nodes as children
;; Node -> Node
(define (make-block-quote-node node)
  (make-node 'block-quote (list node) '() #f))

;; Node -> Boolean
(define (block-quote-node? n)
  (node-type? n 'block-quote))

;; Code block node
;; represents a code block which contains string as children
;; String -> Node
(define (make-code-block-node line)
  (make-node 'code-block (list line) '() #f))

;; Node -> Boolean
(define (code-block-node? n)
  (node-type? n 'code-block))

;; Fenced code node
;; represents a fenced code block which contains a fence type
;; and info-string
;; Data -> Node
(define (make-fenced-code-node data)
  (make-node 'fenced-node #f data #f))

;; Node -> Boolean
(define (fenced-code-node? n)
  (node-type? n 'fenced-code))

;; List node
;; represents a list which only contains item nodes
;; Node Data -> Node 
(define (make-list-node item data)
  (make-node 'list (list item) data #f))

;; Node-> Boolean
(define (list-node? n)
  (node-type? n 'list))

;; Item node
;; represents a item which can only be in a list
;; Node -> Node
(define (make-item-node node)
  (make-node 'item (list node) '() #f))

;; Node -> Boolean
(define (item-node? n)
  (node-type? n 'item))


;; Level is an Integer [1-6]
;; Header node
;; represents either a atx header or setext header
;; String Level -> Node
(define (make-header-node text level)
  (make-node 'header (list (make-text-node text)) `(level . ,level) #t))

;; Node -> Boolean
(define (header-node? n)
  (node-type? n 'header))

;; Text node
;; String Boolean -> Node
(define (make-text-node text)
  (make-node 'text (string-trim-both text) '() #t))

(define (text-node? n)
  (node-type? n 'text))

;; Softbreak node
(define (make-softbreak-node)
  (make-node 'softbreak '() '() #t))

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

(define (add-child-node node child)
  (make-node (node-type node)
             (cons child (node-children node))
             (node-data node)
             (node-closed? node)))

(define (replace-last-child node new-child)
  (make-node (node-type node)
             (cons new-child (rest-children node))
             (node-data node)
             (node-closed? node)))

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
