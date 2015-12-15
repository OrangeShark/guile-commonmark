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
  (let loop ((root (make-document-node))
             (line (read-tabless-line p)))
    (if (eof-object? line)
        root 
        (loop (parse-open-block root line)
              (read-tabless-line p)))))

;; Node String -> Node
(define (parse-open-block n l)
  (cond ((node-closed? n) n)
        ((no-children? n) (let ((parsed-line (parse-line l)))
                            (if parsed-line
                                (add-child-node n parsed-line)
                                n)))
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
        (else (close-node n))))

(define (parse-code-block n l)
  (cond ((code-block? l) => (lambda (rest-line)
                              (replace-last-child n
                                                  (string-append (last-child n)
                                                                 "\n"
                                                                 (match:suffix rest-line)))))
        (else (close-node n))))

(define (parse-paragraph n l)
  (let ((parsed-line (parse-line l)))
    (cond ((not parsed-line)
           (close-node n))
          ((and (setext-header? l) (= (length (node-children n)) 1))
           (make-header-node (node-children (last-child n))
                             (if (string-any #\= l)
                                 1
                                 2)))
          ((paragraph-node? parsed-line)
           (add-child-node (add-child-node n (make-softbreak-node))
                           (last-child parsed-line)))
          (else (close-node n)))))

(define (parse-fenced-code n l)
  (cond ((fenced-code-end? l (cdr (assoc 'fence (node-data n))))
         (close-node n))
        ((no-children? n)
         (add-child-node n l))
        (else (replace-last-child n
                                  (string-append (last-child n)
                                                 "\n"
                                                 l)))))

(define (parse-list-node n l)
  (let ((item (parse-item-node (last-child n) l)))
    (if (node-closed? item)
        n)))

(define (parse-item-node n l)
  n)

;; Node String -> Node
(define (parse-container-block n l)
  (cond ((and (node-closed? (last-child n)) (not (empty-line? l)))
         (add-child-node n (parse-line l)))
        (else (let ((new-child (parse-open-block (last-child n) l)))
                (cond ((and (not (empty-line? l))
                            (node-closed? new-child)
                            (not (fenced-code-node? new-child))
                            (not (header-node? new-child)))
                       (add-child-node (replace-last-child n new-child)
                                       (parse-line l)))
                      (else (replace-last-child n new-child)))))))

(define (header-level s)
  (string-length s))

;; String -> Node
(define (parse-line l)
  (cond ((empty-line? l)          #f)
        ((hrule? l)               (make-hrule))
        ((block-quote? l)         => make-block-quote)
        ((atx-header? l)          => make-atx-header)
        ((code-block? l)          => make-code-block)
        ((fenced-code? l)         => make-fenced-code)
        ((bullet-list-marker? l)  => make-bullet-list-marker)
        ((ordered-list-marker? l) => make-ordered-list-marker)
        (else                     (make-paragraph l))))


(define (make-hrule)
  (make-hrule-node))

(define (make-block-quote match)
  (make-block-quote-node (parse-line (match:suffix match))) )

(define (make-atx-header match)
  (make-header-node (match:suffix match)
                    (header-level (match:substring match 1))))

(define (make-code-block match)
  (make-code-block-node (match:suffix match)))

(define (make-fenced-code match)
  (make-fenced-code-node 
   `((fence . ,(match:substring match 1))
     (info-string . ,(string-trim-both (match:substring match 2))))))

(define (make-bullet-list-marker match)
  (make-list-node (make-item (match:suffix match))
                  `((type . bullet)
                    (tight . #t)
                    (bullet . ,(match:substring match 1)))))

(define (make-ordered-list-marker match)
  (make-list-node (make-item (match:suffix match))
                  `((type . ordered)
                    (start . ,(string->number (match:substring match 1)))
                    (tight . #t)
                    (delimiter . (delimiter-type (match:substring match 2))))))

(define (make-item line)
  (make-item-node (parse-line line)))

(define (make-paragraph line)
  (make-paragraph-node line))


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

