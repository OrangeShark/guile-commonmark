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
(define re-block-quote (make-regexp "^ {0,3}> ?"))
(define re-atx-header (make-regexp "^ {0,3}(#{1,6}) "))
(define re-indented-code-block (make-regexp "^    "))
(define re-setext-header (make-regexp "^ {0,3}(=+|-+) *$"))
(define re-empty-line (make-regexp "^ *$"))
(define re-fenced-code (make-regexp "^ {0,3}(```|~~~)([^`]*)$"))
(define re-bullet-list-marker (make-regexp "^ {0,3}([-+*]) "))
(define re-ordered-list-marker (make-regexp "^ {0,3}([0-9]{1,9})([.)]) "))


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

(define (bullet-list-marker? line)
  (regexp-exec re-bullet-list-marker line))

(define (ordered-list-marker? line)
  (regexp-exec re-ordered-list-marker line))


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
        ((list-node? n) (parse-list-node n l))
        ((item-node? n) (parse-item-node n l))
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
                      (cons (last-child parsed-line)
                            (cons (make-node 'softbreak #f '() #t) (node-children n)))
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
                         (if (node-children n)
                             (list (string-append (last-child n)
                                                  "\n"
                                                  l))
                             (list l))
                         (node-data n)
                         #f))))

(define (parse-list-node n l)
  n)

(define (parse-item-node n l)
  n)

;; Node String -> Node
(define (parse-container-block n l)
 (make-node (node-type n)
            (cond ((and (node-closed? (last-child n)) (not (empty-line? l)))
                   (cons (parse-line l) (node-children n)))
                  (else (let ((new-child (parse-open-block (last-child n) l)))
                          (cond ((and (not (empty-line? l))
                                      (node-closed? new-child)
                                      (not (fenced-code-node? new-child)))
                                 (cons (parse-line l) (cons new-child (rest-children n))))
                                (else (cons new-child (rest-children n)))))))
            (node-data n)
            #f))

(define (header-level s)
  (string-length s))

;; String -> Node
(define (parse-line l)
  (cond ((empty-line? l)         #f)
        ((hrule? l)              (make-hrule))
        ((block-quote? l)         => make-block-quote)
        ((atx-header? l)          => make-atx-header)
        ((code-block? l)          => make-code-block)
        ((fenced-code? l)         => make-fenced-code)
        ((bullet-list-marker? l)  => make-bullet-list-marker)
        ((ordered-list-marker? l) => make-ordered-list-marker)
        (else                     (make-paragraph l))))


(define (make-hrule)
  (make-node 'hrule '() '() #t))

(define (make-block-quote match)
  (make-node 'block-quote
             (list (parse-line (match:suffix match)))
             '()
             #f))

(define (make-atx-header match)
  (make-node 'header
             (list (make-node 'text (match:suffix match) '() #t))
             '()
             #f))

(define (make-code-block match)
  (make-node 'code-block
             (list (match:suffix match))
             '()
             #f))

(define (make-fenced-code match)
  (make-node 'fenced-code
             #f
             `((fence . ,(match:substring match 1))
               (info-string . ,(string-trim-both (match:substring match 2))))
             #f))

(define (make-bullet-list-marker match)
  (make-node 'list
             (list (make-item (match:suffix match)))
             `((type . bullet)
               (tight . #t)
               (bullet . ,(match:substring match 1)))
             #f))

(define (make-ordered-list-marker match)
  (make-node 'list
             (list (make-item (match:suffix match)))
             `((type . ordered)
               (start . ,(string->number (match:substring match 1)))
               (tight . #t)
               (delimiter . (delimiter-type (match:substring match 2))))
             #f))

(define (make-item line)
  (make-node 'item
             (list (parse-line line))
             '()
             #f))

(define (make-paragraph line)
  (make-node 'paragraph
             (list (make-node 'text (string-trim-both line) '() #f))
             '()
             #f))


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

