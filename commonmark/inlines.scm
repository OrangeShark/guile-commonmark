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

(define-module (commonmark inlines)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (commonmark node)
  #:use-module (commonmark common)
  #:export (parse-inlines))

(define re-start-ticks (make-regexp "^`+"))
(define re-ticks (make-regexp "`+"))
(define re-main (make-regexp "^[^`*_\\]+"))

(define (start-ticks? text)
  (regexp-exec re-start-ticks (text-value text) (text-position text)))

(define (end-ticks? text)
  (regexp-exec re-ticks (text-value text) (text-position text)))

(define (normal-text? text)
  (regexp-exec re-main (text-value text) (text-position text)))

(define (match-length match)
  (string-length (match:substring match 0)))

(define (make-text text position)
  (cons text position))

(define (text-value text)
  (car text))

(define (text-position text)
  (cdr text))

(define (text-move text position)
  (make-text (text-value text) position))

(define (text-advance text increment)
  (make-text (text-value text) (+ (text-position text) increment)))

(define (text-substring text start end)
  (substring (text-value text) start end))

(define (text-char text)
  (string-ref (text-value text) (text-position text)))

(define (text-length text)
  (string-length (text-value text)))

(define (text-end? text)
  (>= (text-position text) (string-length (text-value text))))

(define-record-type <delimiter>
  (make-delimiter ch count open close)
  delimiter?
  (ch delimiter-ch)
  (count delimiter-count)
  (open delimiter-open?)
  (close delimiter-close?))

(define (whitespace? text position)
  (or (not position) (char-whitespace? (string-ref text position))))

(define (char-punctuation? ch)
  (char-set-contains? char-set:punctuation ch))

(define (punctuation? text position)
  (and position (char-punctuation? (string-ref text position))))

(define (left-flanking? whitespace-after punctuation-after whitespace-before punctuation-before)
  (and (not whitespace-after)
       (or (not punctuation-after) whitespace-before punctuation-before)))

(define (right-flanking? whitespace-after punctuation-after whitespace-before punctuation-before)
  (and (not whitespace-before)
       (or (not punctuation-before) whitespace-after punctuation-after)))

(define (scan-delim text)
  (define (count-delim delim-end position)
    (- (or delim-end (text-length text)) position))
  (let* ((ch (text-char text))
         (position (text-position text))
         (text (text-value text))
         (delim-end (string-skip text ch position))
         (delim-start (string-skip-right text ch 0 position))
         (whitespace-before (whitespace? text delim-start))
         (whitespace-after (whitespace? text delim-end))
         (punctuation-before (punctuation? text delim-start))
         (punctuation-after (punctuation? text delim-end))
         (left (left-flanking? whitespace-after punctuation-after whitespace-before punctuation-before))
         (right (right-flanking? whitespace-after punctuation-after whitespace-before punctuation-before)))
    (case ch
      ((#\*) (make-delimiter ch (count-delim delim-end position) left right))
      ((#\_) (make-delimiter ch (count-delim delim-end position)
                             (and left (or (not right) punctuation-before))
                             (and right (or (not left) punctuation-after)))))))

(define (match? open-delim close-delim)
  (eq? (delimiter-ch open-delim) (delimiter-ch close-delim)))

(define (matching-opening? delim-stack delim)
  (find (cut match? <> delim) delim-stack))

(define (remake-delimiter count delim)
  (make-delimiter (delimiter-ch delim) count (delimiter-open? delim) (delimiter-close? delim)))

(define (match-delim opening-delim closing-delim)
  (let ((open-count (delimiter-count opening-delim))
        (close-count (delimiter-count closing-delim)))
    (cond ((or (= open-count close-count 1) (= open-count close-count 2))
           (list #f #f))
          ((>= open-count 2 close-count)
           (list (remake-delimiter (- open-count close-count) opening-delim) #f))
          ((<= open-count 2 close-count)
           (list #f (remake-delimiter (- close-count open-count) closing-delim)))
          ((odd? close-count)
           (list (remake-delimiter (- open-count 1) opening-delim)
                 (remake-delimiter (- close-count 1) closing-delim)))
          (else (list (remake-delimiter (- open-count 2) opening-delim)
                      (remake-delimiter (- close-count 2) closing-delim))))))

;; Node -> Node
;; parses the inline text of paragraphs and heading nodes
(define (parse-inlines node)
  (cond ((not (node? node)) node)
        ((or (paragraph-node? node) (heading-node? node)) (parse-inline node))
        (else (make-node (node-type node) (node-data node) (map parse-inlines (node-children node))))))

(define (emphasis-type delim)
  (case (delimiter-count delim)
    ((1) 'em)
    (else 'strong)))

(define (delim->text delim)
  (make-text-node (make-string (delimiter-count delim) (delimiter-ch delim))))

(define (parse-emphasis text nodes delim-stack nodes-stack)
  (define (parse-matching-delim delim matching-delim)
    (let loop ((ds delim-stack)
               (n nodes)
               (ns nodes-stack))
      (if (eq? (car ds) matching-delim)
          (match (match-delim matching-delim delim)
            ((#f #f)
             (parse-char (text-advance text (delimiter-count delim))
                         (cons (make-emphasis-node n (emphasis-type delim)) (car ns))
                         (cdr ds)
                         (cdr ns)))
            ((od #f)
             (parse-char (text-advance text (delimiter-count delim))
                         (list (make-emphasis-node n (emphasis-type delim)))
                         (cons od (cdr ds))
                         ns))
            ((#f cd)
             (parse-char (text-advance text (delimiter-count matching-delim))
                         (cons (make-emphasis-node n (emphasis-type matching-delim)) (car ns))
                         (cdr ds)
                         (cdr ns)))
            ((od cd)
             (let ((difference (- (delimiter-count delim) (delimiter-count cd))))
               (parse-char (text-advance text difference)
                           (list (make-emphasis-node n (if (= 1 difference) 'em 'strong)))
                           (cons od (cdr ds))
                           ns))))
          (loop (cdr ds) (append n (cons (delim->text (car ds)) (car ns))) (cdr ns)))))
  (let ((delim (scan-delim text)))
    (cond ((and (delimiter-close? delim) (delimiter-open? delim))
           (let ((matching-delim (matching-opening? delim-stack delim)))
             (if matching-delim
                 (parse-matching-delim delim matching-delim)
                 (parse-char (text-advance text (delimiter-count delim))
                             '()
                             (cons delim delim-stack)
                             (cons nodes nodes-stack)))))
          ((delimiter-close? delim)
           (let ((matching-delim (matching-opening? delim-stack delim)))
             (if matching-delim
                 (parse-matching-delim delim matching-delim)
                 (parse-char (text-advance text (delimiter-count delim))
                             (cons (delim->text delim) nodes)
                             delim-stack
                             nodes-stack))))
          ((delimiter-open? delim)
           (parse-char (text-advance text (delimiter-count delim))
                       '()
                       (cons delim delim-stack)
                       (cons nodes nodes-stack)))
          (else (parse-char (text-advance text (delimiter-count delim))
                            (cons (delim->text delim) nodes)
                            delim-stack
                            nodes-stack)))))

(define (ascii-punctuation-characters? ch)
  (define ascii-punc-set (string->char-set ascii-punctuation-characters))
  (char-set-contains? ascii-punc-set ch))

(define (parse-backslash text nodes delim-stack nodes-stack)
  (let* ((next-ch-text (text-advance text 1))
         (next-ch (and (not (text-end? next-ch-text)) (text-char next-ch-text))))
    (cond ((eq? next-ch #\newline)
           (parse-char (text-advance next-ch-text 1)
                       (cons (make-hardbreak-node) nodes)
                       delim-stack nodes-stack))
          ((and next-ch (ascii-punctuation-characters? next-ch))
           (parse-char (text-advance next-ch-text 1)
                       (cons (make-text-node (string next-ch)) nodes)
                       delim-stack nodes-stack))
          (else (parse-char next-ch-text (cons (make-text-node "\\") nodes)
                            delim-stack nodes-stack)))))

(define (parse-ticks text nodes delim-stack nodes-stack)
  (let ((start-ticks (start-ticks? text)))
    (let loop ((end-ticks (end-ticks? (text-move text (match:end start-ticks 0)))))
      (cond ((not end-ticks)
             (parse-char (text-move text (match:end start-ticks 0))
                         (cons (make-text-node (match:substring start-ticks 0)) nodes)
                         delim-stack nodes-stack))
            ((= (match-length start-ticks) (match-length end-ticks))
             (parse-char (text-move text (match:end end-ticks 0))
                         (cons (make-code-span-node (text-substring text (match:end start-ticks 0)
                                                                    (match:start end-ticks 0)))
                               nodes)
                         delim-stack nodes-stack))
            (else (loop (end-ticks? (text-move text (match:end end-ticks 0)))))))))

(define (parse-normal-text text nodes delim-stack nodes-stack)
  (let ((normal-text (normal-text? text)))
    (parse-char (text-move text (match:end normal-text 0))
                (cons (make-text-node (match:substring normal-text 0)) nodes)
                delim-stack nodes-stack)))

(define (pop-remaining-delim nodes delim-stack nodes-stack)
  (if (null? delim-stack)
      nodes
      (pop-remaining-delim (append nodes (cons (delim->text (car delim-stack)) (car nodes-stack)))
                           (cdr delim-stack)
                           (cdr nodes-stack))))

(define (parse-char text nodes delim-stack nodes-stack)
  (if (text-end? text)
      (pop-remaining-delim nodes delim-stack nodes-stack)
      (case (text-char text)
        ((#\\) (parse-backslash text nodes delim-stack nodes-stack))
        ((#\`) (parse-ticks text nodes delim-stack nodes-stack))
        ((#\* #\_) (parse-emphasis text nodes delim-stack nodes-stack))
        (else (parse-normal-text text nodes delim-stack nodes-stack)))))

(define (parse-inline node)
  (let ((text (last-child (last-child node))))
    (make-node (node-type node) (node-data node) (parse-char (make-text text 0) '() '() '()))))
