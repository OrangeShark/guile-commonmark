(define-module (commonmark)
  #:use-module (commonmark blocks)
  #:use-module (commonmark node)
  #:use-module (commonmark sxml)
  #:export (commonmark->html
            parse-text))


;; Port -> HTML
;; parses a commonmark document and converts it to HTML
(define (commonmark->xml p)
  (document->xml (parse-inlines (parse-blocks p))))


(define (parse-text s)
  (print-node (parse-blocks (open-input-string s))))
