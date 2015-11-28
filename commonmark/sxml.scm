(define-module (commonmark sxml)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (commonmark node)
  #:export (document->xml))

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
  (cond ((hrule-node? n) (hrule-node->sxml n))
        ((paragraph-node? n) (paragraph-node->sxml n))
        ((block-quote-node? n) (block-quote-node->sxml n))
        ((code-block-node? n) (code-block-node->sxml n))
        ((fenced-code-node? n) (fenced-code-node->sxml n))
        ((header-node? n) (header-node->sxml n))
        ((text-node? n) (text-node->sxml n))
        ((softbreak-node? n) (softbreak-node->sxml n))
        (else (error "unknown node"))))

(define (hrule-node->sxml n)
  '(hr))

(define (paragraph-node->sxml n)
  `(p ,@(fold-nodes node->sxml (node-children n))))

(define (text-node->sxml n)
  (node-children n))

(define (block-quote-node->sxml n)
  `(blockquote ,@(fold-nodes node->sxml (node-children n))))

(define (code-block-node->sxml n)
  `(pre (code ,@(node-children n))))

(define (fenced-code-node->sxml n)
  `(pre (code ,(infostring (assq-ref (node-data n) 'info-string)),@(node-children n))))

(define (header-node->sxml n)
  `(,(level n) ,@(fold-nodes node->sxml (node-children n))))

(define (softbreak-node->sxml n)
  "
")

(define (infostring s)
  `(@ (class ,(string-append "language-" s))))

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
