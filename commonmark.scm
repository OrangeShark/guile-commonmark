(define-module (commonmark)
  #:use-module (commonmark blocks)
  #:use-module (commonmark node)
  #:export (commonmark->html
            parse-text))


;; Port -> HTML
;; parses a commonmark document and converts it to HTML
(define (commonmark->html p)
  (document->html (parse-inlines (parse-blocks p))))


;; Document -> HTML
;; converts the document into HTML
;; !!!

(define (document->html d)
  (... d))


(define (parse-text s)
  (print-node (parse-blocks (open-input-string s))))
