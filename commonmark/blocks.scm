;; Copyright (C) 2015, 2016  Erik Edrosa <erik.edrosa@gmail.com>
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (commonmark node)
  #:use-module (commonmark utils)
  #:use-module (commonmark common)
  #:export (parse-blocks))


(define re-thematic-break (make-regexp "^ {0,3}((\\* *){3,}|(_ *){3,}|(- *){3,}) *$"))
(define re-block-quote (make-regexp "^ {0,3}> ?"))
(define re-atx-heading (make-regexp "^ {0,3}(#{1,6})( .*)?$"))
(define re-atx-heading-end (make-regexp "^(.* )?#+ *$"))
(define re-indented-code-block (make-regexp "^    "))
(define re-setext-heading (make-regexp "^ {0,3}(=+|-+) *$"))
(define re-empty-line (make-regexp "^ *$"))
(define re-fenced-code (make-regexp "^ {0,3}(```+|~~~+)([^`]*)$"))
(define re-bullet-list-marker (make-regexp "^ {0,3}([-+*])( +|$)"))
(define re-ordered-list-marker (make-regexp "^ {0,3}([0-9]{1,9})([.)])( +|$)"))
(define re-link-definition (make-regexp (string-append "^ {0,3}"
                                                       link-label
                                                       ": *\n? *"
                                                       link-destination
                                                       "( +| *\n? *)"
                                                       link-title
                                                       "? *(\n|$)")))


(define (block-quote? l)
  (regexp-exec re-block-quote l))

(define (atx-heading? l)
  (regexp-exec re-atx-heading l))

(define (atx-heading-end? l)
  (regexp-exec re-atx-heading-end l))

(define (code-block? l)
  (regexp-exec re-indented-code-block l))

(define (empty-line? l)
  (regexp-exec re-empty-line l))

(define (thematic-break? line)
  (regexp-exec re-thematic-break line))

(define (setext-heading? line)
  (regexp-exec re-setext-heading line))

(define (fenced-code? line)
  (regexp-exec re-fenced-code line))

(define (fenced-code-end? line fence)
  (string-match (string-append "^ {0,3}" fence "$") line))

(define (bullet-list-marker? line)
  (regexp-exec re-bullet-list-marker line))

(define (ordered-list-marker? line)
  (regexp-exec re-ordered-list-marker line))

(define (link-definition? text)
  (regexp-exec re-link-definition text))


;; Port -> Document
(define (parse-blocks port)
  "Parses CommonMark blocks from PORT returning a CommonMark Document tree"
  (let loop ((root (make-document-node))
             (line (read-line-without-nul port)))
    (if (eof-object? line)
        (parse-clean-up root (lambda (doc references)
                               (if (null? references)
                                   doc
                                   (node-add-data doc 'link-references references))))
        (loop (parse-open-block root line)
              (read-line-without-nul port)))))

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
        ((open-descendant? n 'paragraph) ;; lazy continuation line
         (let ((parsed-line (parse-line l)))
           (if (or (paragraph-node? parsed-line) (code-block-node? parsed-line))
               (parse-container-block n l)
               (close-node n))))
        (else (close-node n))))

(define (parse-code-block n l)
  (cond ((code-block? l) => (lambda (rest-line)
                              (add-child-node n (match:suffix rest-line))))
        ((empty-line? l) (add-child-node n ""))
        (else (close-node n))))

(define (parse-paragraph n l)
  (let ((parsed-line (parse-line l)))
    (cond ((blank-node? parsed-line)
           (close-node n))
          ((and (setext-heading? l) (= (length (node-children (last-child n))) 1))
           (make-heading-node (last-child (last-child n))
                              (if (string-any #\= l) 1 2)))
          ((paragraph-node? parsed-line)
           (replace-last-child n (join-text-nodes (last-child n) (last-child parsed-line))))
          ((code-block-node? parsed-line)
           (replace-last-child n (add-text (last-child n) l)))
          (else (close-node n)))))

(define (remove-min-spaces l n)
  (let ((space-end (string-index l (lambda (c) (not (eq? #\space c))))))
    (if space-end
        (substring l (min n space-end))
        (substring l (min n (string-length l))))))

(define (fence-start n)
  (node-get-data n 'fence-start))

(define (parse-fenced-code n l)
  (cond ((fenced-code-end? l (node-get-data n 'fence))
         (close-node n))
        ((no-children? n)
         (add-child-node n (remove-min-spaces l (fence-start n))))
        (else (replace-last-child n
                                  (string-append (last-child n)
                                                 "\n"
                                                 (remove-min-spaces l (fence-start n)))))))

(define (list-type n)
  (node-get-data n 'type))

(define (list-bullet n)
  (node-get-data n 'bullet))

(define (list-delimiter n)
  (node-get-data n 'delimiter))

(define (eq-list-types? l1 l2)
  (or (and (eq? (list-type l1) (list-type l2) 'bullet)
           (string=? (list-bullet l1) (list-bullet l2)))
      (and (eq? (list-type l1) (list-type l2) 'ordered)
           (string=? (list-delimiter l1) (list-delimiter l2)))))

(define (parse-list n l)
  (define (remove-blank-node item)
    (if (blank-last-child? item)
        (remove-last-child item)
        item))

  (define (parse-new-item n l item)
    (let ((new-list (parse-line l)))
      (cond ((and (list-node? new-list)
                  (eq-list-types? n new-list))
             (add-child-node (replace-last-child n item)
                             (last-child new-list)))
            (else (close-node (replace-last-child n (remove-blank-node item)))))))

  (if (node-closed? (last-child n))
      (parse-new-item n l (last-child n))
      (let ((item (parse-item (last-child n) l)))
        (cond ((and (node-closed? item) (empty-line? l))
               (close-node (replace-last-child n item)))
              ((node-closed? item)
               (parse-new-item n l item))
              (else (replace-last-child n item))))))

(define (n-spaces? n s)
  (>= (string-index s (lambda (c) (not (char=? #\space c)))) n))

(define (item-padding node)
  (node-get-data node 'padding))

(define (blank-last-child? node)
  (and (not (no-children? node)) (blank-node? (last-child node))))

(define (parse-item n l)
  (let ((padding (item-padding n)))
    (cond ((and (blank-last-child? n) (= (length (node-children n)) 1))
           (close-node n))
          ((and (empty-line? l) (no-children? n))
           (add-child-node n (make-blank-node)))
          ((and (empty-line? l) (blank-node? (last-child n)))
           (close-node (remove-last-child n)))
          ((empty-line? l)
           (cond ((open-descendant? n 'fenced-code)
                  (parse-container-block n l))
                 ((list-node? (last-child n))
                  (let ((container (parse-container-block n l)))
                    (if (node-closed? (last-child container))
                        (close-node container)
                        (add-child-node container (make-blank-node)))))
                 ((open-descendant? n 'code-block)
                  (add-child-node (parse-container-block n l)
                                  (make-blank-node)))
                 (else (add-child-node (replace-last-child n (close-node (last-child n)))
                                       (make-blank-node)))))
          ((n-spaces? padding l)
           (let* ((new-line (substring l padding))
                  (node (if (and (blank-last-child? n) (or (prev-node? n 'code-block) (prev-node? n 'list)))
                            (remove-last-child n)
                            n))
                  (new-item (parse-container-block node new-line)))
             (if (and (blank-last-child? n) (prev-node? new-item 'list) (prev-node-closed? new-item))
                 (add-child-node (replace-last-child new-item (last-child n)) (last-child new-item))
                 new-item)))
          ((or (no-children? n) (blank-node? (last-child n)))
           (close-node n))
          ((open-descendant? n 'paragraph)
           (let ((parsed-line (parse-line (remove-min-spaces l padding))))
             (if (paragraph-node? parsed-line)
                 (parse-container-block n l)
                 (close-node (replace-last-child n (close-node (last-child n)))))))
          (else (close-node (replace-last-child n (close-node (last-child n))))))))

;; Node String -> Node
(define (parse-container-block n l)
  (cond ((or (no-children? n) (and (node-closed? (last-child n)) (not (empty-line? l))))
         (if (empty-line? l) n (add-child-node n (parse-line l))))
        (else (let ((new-child (parse-open-block (last-child n) l)))
                (cond ((and (not (empty-line? l))
                            (node-closed? new-child)
                            (not (fenced-code-node? new-child))
                            (not (heading-node? new-child)))
                       (add-child-node (replace-last-child n new-child)
                                       (parse-line l)))
                      (else (replace-last-child n new-child)))))))

(define (heading-level s)
  (string-length s))

;; String -> Node
(define (parse-line l)
  (cond ((empty-line? l)             (make-blank-node))
        ((thematic-break? l)         (make-thematic-break))
        ((block-quote? l)         => make-block-quote)
        ((atx-heading? l)         => make-atx-heading)
        ((code-block? l)          => make-code-block)
        ((fenced-code? l)         => make-fenced-code)
        ((bullet-list-marker? l)  => make-bullet-list-marker)
        ((ordered-list-marker? l) => make-ordered-list-marker)
        (else                        (make-paragraph l))))


(define (make-thematic-break)
  (make-thematic-break-node))

(define (make-block-quote match)
  (make-block-quote-node (parse-line (match:suffix match))))

(define (make-atx-heading match)
  (let* ((text (or (match:substring match 2) ""))
         (end (atx-heading-end? text)))
    (if end
        (make-heading-node (or (match:substring end 1) "") (heading-level (match:substring match 1)))
        (make-heading-node text (heading-level (match:substring match 1))))))

(define (make-code-block match)
  (make-code-block-node (match:suffix match)))

(define (make-fenced-code match)
  (make-fenced-code-node 
   `((fence . ,(match:substring match 1))
     (fence-start . ,(match:start match 1))
     (info-string . ,(unescape-string (string-trim-both (match:substring match 2)))))))

(define (make-bullet-list-marker match)
  (make-list-node (make-item (match:suffix match) (match:end match 1) (match:substring match 2))
                  `((type . bullet)
                    (tight . #t)
                    (bullet . ,(match:substring match 1)))))

(define (make-ordered-list-marker match)
  (make-list-node (make-item (match:suffix match) (match:end match 2) (match:substring match 3))
                  `((type . ordered)
                    (start . ,(string->number (match:substring match 1)))
                    (tight . #t)
                    (delimiter . ,(match:substring match 2)))))

(define (make-item line offset spaces)
  (let ((padding (string-length spaces)))
    (cond ((>= padding 5)
           (make-item-node (parse-line (string-append (substring spaces 1) line))
                           (+ offset 1)))
          ((< padding 1)
           (make-item-node #f (+ offset 1)))
          (else (make-item-node (parse-line line) (+ offset padding))))))

(define (make-paragraph line)
  (make-paragraph-node line))

(define (parse-clean-up n col)
  (cond ((not (node? n)) (col n '()))
        ((code-block-node? n) (remove-empty-lines n col))
        ((paragraph-node? n) (parse-reference-definition n col))
        ((list-node? n) (clean-list-nodes n col))
        ((item-node? n) (remove-blank-nodes n col))
        (else (filter-map&co parse-clean-up (node-children n)
                             (lambda (v d)
                               (col (make-node (node-type n)
                                               (node-data n)
                                               v)
                                    d))))))

(define (reverse-join ls)
  (reduce (cut string-append <> "\n" <>) "" ls))

(define (parse-reference-definition n col)
  (let loop ((text (reverse-join (node-children (last-child n))))
             (links '()))
    (cond ((link-definition? text) =>
           (lambda (match)
             (loop (match:suffix match)
                   (cons (list (string-map char-downcase (match:substring match 1))
                               (match:substring match 3)
                               (match:substring match 7))
                         links))))
          (else (col (if (= (string-length text) 0)
                         #f
                         (make-paragraph text)) links)))))

(define (remove-empty-lines n col)
  (col (make-node (node-type n) (node-data n)
                  (list (reverse-join (drop-while empty-line?
                                                  (node-children n)))))
       '()))

(define (clean-list-nodes node col)
  (define (any-blank-nodes? item)
    (any blank-node? (node-children item)))

  (let ((blank-nodes (any any-blank-nodes? (node-children node))))
    (filter-map&co parse-clean-up (node-children node)
                   (lambda (v d)
                     (col (make-node (node-type node)
                                     (if blank-nodes
                                         (acons 'tight #f (node-data node))
                                         (node-data node))
                                     v)
                          d)))))

(define (remove-blank-nodes n col)
  (cond ((no-children? n) (col n '()))
        (else (filter-map&co parse-clean-up (filter (negate blank-node?) (node-children n))
                             (lambda (v d)
                               (col (make-node (node-type n)
                                               (node-data n)
                                               v)
                                    d))))))
