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

(define-module (commonmark sxml)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (commonmark node)
  #:export (document->sxml))

;; Document -> xml
;; converts the document into HTML
;; !!!

(define (document->xml d)
  (sxml->xml (document->sxml d)))


(define (document->sxml d)
  (if (document-node? d)
      (fold (lambda (elem prev) (cons (node->sxml elem) prev)) '() (node-children d))
      (error "not a document node")))

(define (node->sxml n)
  (cond ((thematic-break-node? n) (thematic-break-node->sxml n))
        ((paragraph-node? n) (paragraph-node->sxml n))
        ((block-quote-node? n) (block-quote-node->sxml n))
        ((code-block-node? n) (code-block-node->sxml n))
        ((fenced-code-node? n) (fenced-code-node->sxml n))
        ((heading-node? n) (heading-node->sxml n))
        ((list-node? n) (list-node->sxml n))
        ((text-node? n) (text-node->sxml n))
        ((code-span-node? n) (code-span-node->sxml n))
        ((softbreak-node? n) (softbreak-node->sxml n))
        ((hardbreak-node? n) (hardbreak-node->sxml n))
        ((emphasis-node? n) (emphasis-node->sxml n))
        ((link-node? n) (link-node->sxml n))
        ((image-node? n) (image-node->sxml n))
        (else (error "unknown node"))))

(define (thematic-break-node->sxml n)
  '(hr))

(define (paragraph-node->sxml n)
  `(p ,@(fold-nodes node->sxml (node-children n))))

(define (text-node->sxml n)
  (last-child n))

(define (code-span-node->sxml n)
  `(code ,@(node-children n)))

(define (block-quote-node->sxml n)
  `(blockquote ,@(fold-nodes node->sxml (node-children n))))

(define (code-block-node->sxml n)
  `(pre (code ,@(node-children n))))

(define (fenced-code-node->sxml n)
  `(pre (code ,(infostring (assq-ref (node-data n) 'info-string)),@(node-children n))))

(define (heading-node->sxml n)
  `(,(level n) ,@(fold-nodes node->sxml (node-children n))))

(define (list-type n)
  (case (assq-ref (node-data n) 'type)
    ((bullet) 'ul)
    (else 'ol)))

(define (list-tight? node)
  (assq-ref (node-data node) 'tight))

(define (list-node->sxml n)
  `(,(list-type n) ,@(if (list-tight? n)
                         (fold-nodes tight-item-node->sxml (node-children n))
                         (fold-nodes item-node->sxml (node-children n)))))

(define (item-node->sxml n)
  `(li ,@(fold-nodes node->sxml (node-children n))))

(define (tight-item-node->sxml node)
  `(li ,@(if (paragraph-node? (last-child node))
             (fold-nodes node->sxml (node-children (last-child node)))
             (fold-nodes node->sxml (node-children node)))))

(define (softbreak-node->sxml n)
  "\n")

(define (hardbreak-node->sxml n)
  '(br))

(define (emphasis-type n)
  (case (assq-ref (node-data n) 'type)
    ((em) 'em)
    (else 'strong)))

(define (emphasis-node->sxml n)
  `(,(emphasis-type n) ,@(fold-nodes node->sxml (node-children n))))

(define (destination node)
  (assq-ref (node-data node) 'destination))

(define (title node)
  (assq-ref (node-data node) 'title))

(define (link-node->sxml node)
  (let ((dest (destination node))
        (title (title node)))
    `(a (@ (href ,dest) ,@(if title (list (list 'title title)) '()))
        ,@(fold-nodes node->sxml (node-children node)))))

(define (fold-text node)
  (fold (lambda (elem prev)
          (append (node->text elem) prev))
        '()
        (node-children node)))

(define (node->text node)
  (if (text-node? node)
      (node-children node)
      (fold-text node)))

(define (alt-text node)
  (string-concatenate (fold-text node)))

(define (image-node->sxml node)
  (let ((dest (destination node))
        (title (title node)))
    `(img (@ (src ,dest) (alt ,(alt-text node)) ,@(if title (list (list 'title title)) '())))))

(define (infostring s)
  (let ((language (string-trim-both s)))
    (if (string-null? language)
        '(@)
        `(@ (class ,(string-append "language-" language))))))

(define (level n)
  (case (assq-ref (node-data n) 'level)
    ((1) 'h1)
    ((2) 'h2)
    ((3) 'h3)
    ((4) 'h4)
    ((5) 'h5)
    ((6) 'h6)))

(define (fold-nodes f ns)
  (fold (lambda (elem prev) (cons (f elem) prev)) '() ns))
