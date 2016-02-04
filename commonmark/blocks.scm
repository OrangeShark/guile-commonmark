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
  #:export (parse-blocks))

;; ']' needs to be the first character after an openning '[' to be able
;; to match ']'
(define ascii-punctuation-characters "[]!\"#$%&'()*+,-./:;<=>?@[\\^_`{|}~]")
(define escaped-characters (string-append "\\\\" ascii-punctuation-characters))
(define regular-characters "[^\x01-\x19 ()\\\\]")
(define link-label (string-append "\\[(([^][]|"
                                     escaped-characters
                                     "){1,1000})\\]"))
(define link-destination (string-append "((" regular-characters "+|"
                                        escaped-characters ")+)"))
(define link-title (string-append "((\"(" escaped-characters "|[^\"])*\"|"
                                  "'(" escaped-characters "|[^'])*'))"))

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
  (string-match (string-append "^" fence "$") line))

(define (bullet-list-marker? line)
  (regexp-exec re-bullet-list-marker line))

(define (ordered-list-marker? line)
  (regexp-exec re-ordered-list-marker line))

(define (link-definition? text)
  (regexp-exec re-link-definition text))


;; Port -> Document
;; parses commonmark by blocks and creates a document containing blocks
;; !!!
(define (parse-blocks p)
  (let loop ((root (make-document-node))
             (line (read-tabless-line p)))
    (if (eof-object? line)
        (parse-clean-up root (lambda (doc references)
                               (if (null? references)
                                   doc
                                   (node-add-data doc 'link-references references))))
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
                              (add-child-node n (match:suffix rest-line))))
        ((empty-line? l) (add-child-node n ""))
        (else (close-node n))))

(define (parse-paragraph n l)
  (let ((parsed-line (parse-line l)))
    (cond ((not parsed-line)
           (close-node n))
          ((and (setext-heading? l) (= (length (node-children (last-child n))) 1))
           (make-heading-node (last-child (last-child n))
                              (if (string-any #\= l) 1 2)))
          ((paragraph-node? parsed-line)
           (replace-last-child n (join-text-nodes (last-child n) (last-child parsed-line))))
          ((code-block-node? parsed-line)
           (replace-last-child n (add-text (last-child n) l)))
          (else (close-node n)))))

(define (parse-fenced-code n l)
  (cond ((fenced-code-end? l (node-get-data n 'fence))
         (close-node n))
        ((no-children? n)
         (add-child-node n l))
        (else (replace-last-child n
                                  (string-append (last-child n)
                                                 "\n"
                                                 l)))))

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
  (let ((padding (node-get-data n 'padding)))
    (cond ((and (no-children? n)
                (empty-line? l))
           (close-node n))
          ((and (not (no-children? n))
                (node-closed? (last-child n))
                (empty-line? l))
           (close-node n))
          ((empty-line? l)
           (if (fenced-code-node? (last-child n))
               (replace-last-child n (parse-fenced-code (last-child n) l))
               (replace-last-child n (close-node (last-child n)))))
          ((n-spaces? padding l)
           (parse-container-block n (substring l padding)))
          ((no-children? n)
           (close-node n))
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
  (cond ((empty-line? l)          #f)
        ((thematic-break? l)         (make-thematic-break))
        ((block-quote? l)         => make-block-quote)
        ((atx-heading? l)          => make-atx-heading)
        ((code-block? l)          => make-code-block)
        ((fenced-code? l)         => make-fenced-code)
        ((bullet-list-marker? l)  => make-bullet-list-marker)
        ((ordered-list-marker? l) => make-ordered-list-marker)
        (else                     (make-paragraph l))))


(define (make-thematic-break)
  (make-thematic-break-node))

(define (make-block-quote match)
  (make-block-quote-node (parse-line (match:suffix match))) )

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
     (info-string . ,(string-trim-both (match:substring match 2))))))

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
           (make-item-node (parse-line (string-append (substring spaces 1) line)) (+ offset 1)))
          ((< padding 1)
           (make-item-node #f (+ offset 1)))
          (else (make-item-node (parse-line line) (+ offset padding))))))

(define (make-paragraph line)
  (make-paragraph-node line))

(define (parse-clean-up n col)
  (cond ((not (node? n)) (col n '()))
        ((code-block-node? n) (remove-empty-lines n col))
        ((paragraph-node? n) (parse-reference-definition n col))
        (else (map&co parse-clean-up (node-children n)
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
                   (cons (list (match:substring match 1)
                               (match:substring match 3)
                               (match:substring match 6))
                         links))))
          (else (col (make-paragraph text) links)))))

(define (remove-empty-lines n col)
  (col (make-node (node-type n) (node-data n)
                  (list (reverse-join (drop-while empty-line?
                                                  (node-children n)))))
       '()))
