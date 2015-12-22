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
(define re-bullet-list-marker (make-regexp "^ {0,3}([-+*])( +|$)"))
(define re-ordered-list-marker (make-regexp "^ {0,3}([0-9]{1,9})([.)])( +|$)"))


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
        ((document-node? n) (parse-container-block n l))
        ((block-quote-node? n) (parse-block-quote n l))
        ((code-block-node? n) (parse-code-block n l))
        ((fenced-code-node? n) (parse-fenced-code n l))
        ((list-node? n) (parse-list n l))
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
           (replace-last-child n (join-text-nodes (last-child n) (last-child parsed-line))))
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

(define (list-type n)
  (assq-ref (node-data n) 'type))

(define (list-bullet n)
  (assq-ref (node-data n) 'bullet))

(define (list-delimiter n)
  (assq-ref (node-data n) 'delimiter))

(define (eq-list-types? l1 l2)
  (or (and (eq? (list-type l1) (list-type l2) 'bullet)
           (string=? (list-bullet l1) (list-bullet l2)))
      (and (eq? (list-type l1) (list-type l2) 'ordered)
           (string=? (list-delimiter l1) (list-delimiter l2)))))

(define (parse-list n l)
  (let ((item (parse-item (last-child n) l)))
    (cond ((and (node-closed? item) (empty-line? l))
           (close-node (replace-last-child n item)))
          ((node-closed? item)
           (let ((new-list (parse-line l)))
             (cond ((and (list-node? new-list)
                         (eq-list-types? n new-list))
                    (add-child-node (replace-last-child n item)
                                    (last-child new-list)))
                   (else (close-node (replace-last-child n item))))))
          (else (replace-last-child n item)))))

(define (n-spaces? n s)
  (>= (string-index s (lambda (c) (not (char=? #\space c)))) n))

(define (parse-item n l)
  (let ((padding (assq-ref (node-data n) 'padding)))
    (cond ((and (not (no-children? n))
                (node-closed? (last-child n))
                (empty-line? l))
           (close-node n))
          ((empty-line? l)
           (if (fenced-code-node? (last-child n))
               (replace-last-child n (parse-fenced-code (last-child n) l))
               (replace-last-child n (close-node (last-child n)))))
          ((n-spaces? padding l)
           (parse-container-block n (substring l padding)))
          (else (close-node n)))))

;; Node String -> Node
(define (parse-container-block n l)
  (cond ((or (no-children? n) (and (node-closed? (last-child n)) (not (empty-line? l))))
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
  (make-list-node (make-item (match:suffix match) 1 (match:substring match 2))
                  `((type . bullet)
                    (tight . #t)
                    (bullet . ,(match:substring match 1)))))

(define (make-ordered-list-marker match)
  (make-list-node (make-item (match:suffix match) 2 (match:substring match 3))
                  `((type . ordered)
                    (start . ,(string->number (match:substring match 1)))
                    (tight . #t)
                    (delimiter . ,(match:substring match 2)))))

(define (make-item line offset spaces)
  (let ((padding (string-length spaces)))
    (cond ((>= padding 5)
           (make-item-node (parse-line (string-append (substring spaces 1) line)) (+ offset 1)))
          ((< padding 1)
           (make-item-node #f (+ offset 1)))
          (else (make-item-node (parse-line line) (+ offset padding))))))

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

