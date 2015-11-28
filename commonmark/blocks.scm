(define-module (commonmark blocks) 
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (commonmark node)
  #:export (parse-blocks))

;; String -> String
;; Expands tabs in the string into spaces
(define (expand-tabs line)
  (list->string 
   (let loop ((l (string->list line)) (c 1))
     (cond [(null? l) '()]
           [(char=? #\tab (car l))
            (if (= (remainder c 4) 0)
                (cons #\space
                      (loop (cdr l) (1+ c)))
                (cons #\space
                      (loop l (1+ c))))]
           [else (cons (car l)
                       (loop (cdr l) (1+ c)))]))))

(define re-hrule (make-regexp "^((\\* *){3,}|(_ *){3,}|(- *){3,}) *$"))
(define re-block-quote (make-regexp "^ {0,3}>"))
(define re-atx-header (make-regexp "^ {0,3}(#{1,6}) "))
(define re-indented-code-block (make-regexp "^    "))
(define re-setext-header (make-regexp "^ {0,3}(=+|-+) *$"))
(define re-empty-line (make-regexp "^ *$"))
(define re-fenced-code (make-regexp "^ {0,3}(```|~~~)([^`]*)$"))


(define (block-quote? l)
  (regexp-exec re-block-quote l))

(define (atx-header? l)
  (regexp-exec re-atx-header l))

(define (code-block? l)
  (regexp-exec re-indented-code-block l))

(define (empty-line? l)
  (regexp-exec re-empty-line l))

(define (hrule? line)
  (regexp-exec re-hrule line))

(define (setext-header? line)
  (regexp-exec re-setext-header line))

(define (fenced-code? line)
  (regexp-exec re-fenced-code line))

(define (fenced-code-end? line fence)
  (string-match fence line))


;; Port -> Document
;; parses commonmark by blocks and creates a document containing blocks
;; !!!
(define (parse-blocks p)
  (let loop ((root (make-node 'document '() '() #f))
             (line (read-tabless-line p)))
    (if (eof-object? line)
        root 
        (loop (parse-open-block root line)
              (read-tabless-line p)))))

;; Node String -> Node
(define (parse-open-block n l)
  (cond ((node-closed? n) n)
        ((no-children? n) (make-node (node-type n)
                                     (let ((parsed-line (parse-line l)))
                                       (if parsed-line
                                           (list (parse-line l))
                                           '()))
                                     (node-data n)
                                     #f))
        ((document-node? n) (parse-container-block n l))
        ((block-quote-node? n) (parse-block-quote n l))
        ((code-block-node? n) (parse-code-block n l))
        ((fenced-code-node? n) (parse-fenced-code n l))
        ((paragraph-node? n) (parse-paragraph n l))))

(define (parse-block-quote n l)
  (cond ((block-quote? l) => (lambda (rest-line)
                               (parse-container-block n (match:suffix rest-line))))
        (else (make-node 'block-quote (node-children n) (node-data n) #t))))

(define (parse-code-block n l)
  (cond ((code-block? l) => (lambda (rest-line)
                              (make-node 'code-block
                                         (list (string-append (last-child n)
                                                              "\n"
                                                              (match:suffix rest-line)))
                                         (node-data n)
                                         #f)))
        (else (make-node 'code-block (node-children n) (node-data n) #t))))

(define (parse-paragraph n l)
  (let ((parsed-line (parse-line l)))
    (cond ((not parsed-line)
           (make-node 'paragraph
                      (node-children n)
                      (node-data n)
                      #t))
          ((and (setext-header? l) (= (length (node-children n)) 1))
           (make-node 'header
                      (node-children n)
                      `((level . ,(if (string-any #\= l)
                                      1
                                      2)))
                      #f))
          ((paragraph-node? parsed-line)
           (make-node 'paragraph
                      (cons (last-child parsed-line) (node-children n))
                      (node-data n)
                      #f))
          (else (make-node 'paragraph (node-children n) (node-data n) #t)))))

(define (parse-fenced-code n l)
  (cond ((fenced-code-end? l (cdr (assoc 'fence (node-data n))))
         (make-node 'fenced-code
                    (node-children n)
                    (node-data n)
                    #t))
        (else (make-node 'fenced-code
                         (list (string-append (last-child n)
                                              "\n"
                                              l))
                         (node-data n)
                         #f))))

;; Node String -> Node
(define (parse-container-block n l)
  (let ((new-child (parse-open-block (last-child n) l)))
    (make-node (node-type n) 
               (cond ((and (not (empty-line? l)) (node-closed? new-child) (not (fenced-code-node? new-child))) 
                      (cons (parse-line l) (cons new-child (rest-children n))))
                     (else (cons new-child (rest-children n))))
               (node-data n)
               #f)))

(define (header-level s)
  (string-length s))

;; String -> Node
(define (parse-line l)
  (cond ((empty-line? l) #f)
        ((hrule? l) (make-node 'hrule '() '() #t))
        ((block-quote? l) => (lambda (s) 
                               (make-node 'block-quote 
                                          (list (parse-line (match:suffix s)))
                                          '()
                                          #f)))
        ((atx-header? l) => (lambda (s)
                              (make-node 'header
                                         (list (make-node 'text (match:suffix s) '() #t))
                                         `((level . ,(header-level (match:substring s 1))))
                                         #t)))
        ((code-block? l) => (lambda (s)
                              (make-node 'code-block
                                         (list (match:suffix s))
                                         '()
                                         #f)))
        ((fenced-code? l) => (lambda (s)
                               (make-node 'fenced-code
                                          '("")
                                          `((fence . ,(match:substring s 1))
                                            (info-string . ,(match:substring s 2)))
                                          #f)))
        (else (make-node 'paragraph (list (make-node 'text l '() #f)) '() #f))))

;; Line is one of:
;;  - String
;;  - eof-object

;; Port -> Line
;; read a line from port p and expands any tabs
(define (read-tabless-line p)
  (let ((line (read-line p)))
    (if (eof-object? line)
        line 
        (expand-tabs line))))

