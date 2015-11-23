(define-module (commonmark node)
  #:use-module (srfi srfi-9)
  #:export (make-node
            node-type
            node-children
            node-data
            node-closed?
            no-children?
            block-quote-node?
            document-node?
            paragraph-node?
            code-block-node?
            fenced-code-node?
            child-closed?
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
;; - 'atx-header
;; - 'setext-header
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

;; Node -> Boolean
(define (block-quote-node? n)
  (equal? (node-type n) 'block-quote))

(define (document-node? n)
  (equal? (node-type n) 'document))

(define (paragraph-node? n)
  (equal? (node-type n) 'paragraph))

(define (code-block-node? n)
  (equal? (node-type n) 'code-block))

(define (fenced-code-node? n)
  (equal? (node-type n) 'fenced-code))

(define (child-closed? n)
  (node-closed? (last-child n)))

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
