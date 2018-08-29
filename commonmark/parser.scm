;; Copyright (C) 2016-2018  Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (commonmark parser)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (commonmark common)
  #:export (make-parser
            parser-str
            parser-pos
            parser-col
            parser-end?
            parser-advance
            parser-advance-next-nonspace
            parser-advance-min-spaces
            code-indent
            parser-indented?
            parser-rest-str
            block-quote
            block-quote-rest
            atx-heading
            atx-heading-content
            atx-heading-opening
            empty-line
            thematic-break
            setext-heading
            fenced-code
            fenced-code-fence
            fenced-code-start
            fenced-code-info-string
            fenced-code-end
            bullet-list-marker
            bullet-list-rest
            bullet-list-offset
            bullet-list-spaces
            bullet-list-bullet
            ordered-list-marker
            ordered-list-rest
            ordered-list-offset
            ordered-list-spaces
            ordered-list-number
            ordered-list-delimiter
            link-definition
            link-definition-rest
            link-definition-label
            link-definition-destination
            link-definition-title))

(define-record-type <parser>
  (%make-parser str pos col)
  parser?
  (str parser-str)
  (pos parser-pos)
  (col parser-col))

(define (make-parser str)
  (%make-parser str 0 0))

(define (parser-char=? parser ch)
  (char=? (string-ref (parser-str parser) (parser-pos parser))
          ch))

(define (parser-end? parser)
  (>= (parser-pos parser) (string-length (parser-str parser))))

(define (parser-advance parser offset)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser))
               (count offset))
      (cond ((>= pos (string-length str))
                (%make-parser str pos col))
            ((<= count 0)
             (%make-parser str pos col))
            ((char=? (string-ref str pos) #\tab)
             (let ((col-change (- 4 (modulo col 4))))
               (if (>= count col-change)
                   (loop (+ pos 1) (+ col col-change) (- count col-change))
                   (%make-parser str pos (+ col count)))))
            (else (loop (+ pos 1) (+ col 1) (- count 1)))))))

(define (parser-advance-optional parser ch)
  (define new-parser (cut %make-parser
                          (parser-str parser)
                          (+ (parser-pos parser) 1)
                          <>))
  (if (and (not (parser-end? parser)) (parser-char=? parser ch))
      (new-parser (+ (parser-col parser)
                     (case ch
                       ((#\tab) (- 4 (modulo (parser-col parser) 4)))
                       (else 1))))
      parser))

(define (parser-advance-next-nonspace parser)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser)))
      (if (>= pos (string-length str))
          (%make-parser str pos col)
          (case (string-ref str pos)
            ((#\space) (loop (+ pos 1) (+ col 1)))
            ((#\tab)   (loop (+ pos 1) (+ col (- 4 (modulo col 4)))))
            (else (%make-parser str pos col)))))))

(define (parser-advance-min-spaces parser n)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser))
               (count n))
      (cond ((>= pos (string-length str))
             (%make-parser str pos col))
            ((<= count 0)
             (%make-parser str pos col))
            ((char=? (string-ref str pos) #\space)
             (loop (+ pos 1) (+ col 1) (- count 1)))
            ((char=? (string-ref str pos) #\tab)
             (let ((col-change (- 4 (modulo col 4))))
               (if (>= count col-change)
                   (loop (+ pos 1) (+ col col-change) (- count col-change))
                   (%make-parser str pos (+ col count)))))
            (else (%make-parser str pos col))))))

(define code-indent 4)

(define (parser-indented? start end)
  (>= (- (parser-col end) (parser-col start)) code-indent))

(define (parser-rest-str parser)
  (let ((str (parser-str parser))
        (pos (parser-pos parser)))
    (if (or (>= pos (string-length str))
            (not (char=? (string-ref str pos) #\tab)))
        (substring str pos)
        (let* ((col (parser-col parser))
               (expand (- 4 (modulo col 4))))
          (if (= expand 0)
              (substring str pos)
              (string-append (make-string expand #\space) (substring str (+ pos 1))))))))

(define re-thematic-break (make-regexp "^((\\*[ \t]*){3,}|(_[ \t]*){3,}|(-[ \t]*){3,})[ \t]*$"))
(define re-atx-heading (make-regexp "^(#{1,6})([ \t]+|$)"))
(define re-atx-heading-end (make-regexp "([ \t]+#+[ \t]*)$|(^#+[ \t]*)$"))
(define re-setext-heading (make-regexp "^(=+|-+)[ \t]*$"))
(define re-empty-line (make-regexp "^[ \t]*$"))
(define re-fenced-code (make-regexp "^(```+|~~~+)([^`]*)$"))
(define re-bullet-list-marker (make-regexp "^([-+*])([ \t]|$)"))
(define re-ordered-list-marker (make-regexp "^([0-9]{1,9})([.)])([ \t]|$)"))
(define re-link-definition (make-regexp (string-append "^"
                                                       link-label
                                                       ":[ \t\v]*\n?[ \t\v]*"
                                                       link-destination
                                                       "([ \t\v]+|[ \t\v]*\n?[ \t\v]*)"
                                                       link-title
                                                       "?[ \t\v]*(\n|$)")))
(define re-link-label (make-regexp (string-append "^" link-label ":")))
(define re-link-destination-brackets (make-regexp (string-append "^<(([^ <>\n\t\\]|"
                                                                 escaped-characters
                                                                 ")*)>")))
(define re-link-destination (make-regexp link-destination))
(define re-link-title (make-regexp link-title))


(define (block-quote parser)
  (if (and (not (parser-end? parser)) (parser-char=? parser #\>))
      (parser-advance parser 1)
      #f))

(define (block-quote-rest parser)
  (if (and (not (parser-end? parser))
           (or (parser-char=? parser #\space)
               (parser-char=? parser #\tab)))
      (parser-advance parser 1)
      parser))

(define (atx-heading parser)
  (regexp-exec re-atx-heading (parser-str parser) (parser-pos parser)))

(define (atx-heading-content match)
  (let ((end-match (regexp-exec re-atx-heading-end (match:suffix match))))
    (if end-match
        (match:prefix end-match)
        (match:suffix match))))

(define (atx-heading-opening match)
  (match:substring match 1))

(define (fenced-code-fence match)
  (match:substring match 1))

(define (fenced-code-start match)
  (match:start match 1))

(define (fenced-code-info-string match)
  (match:substring match 2))

(define (empty-line parser)
  (regexp-exec re-empty-line (parser-str parser) (parser-pos parser)))

(define (thematic-break parser)
  (regexp-exec re-thematic-break (parser-str parser) (parser-pos parser)))

(define (setext-heading parser)
  (regexp-exec re-setext-heading (parser-str parser) (parser-pos parser)))

(define (fenced-code parser)
  (regexp-exec re-fenced-code (parser-str parser) (parser-pos parser)))

(define (fenced-code-end parser fence)
  (string-match (string-append "^" fence "+$") (parser-str parser) (parser-pos parser)))

(define (bullet-list-marker parser)
  (regexp-exec re-bullet-list-marker (parser-str parser) (parser-pos parser)))

(define (bullet-list-rest parser match)
  (parser-advance parser (- (match:end match 1) (parser-col parser))))

(define (bullet-list-offset parser parser-rest)
  (- (parser-col parser-rest) (parser-col parser)))

(define (bullet-list-spaces parser)
  (- (parser-col (parser-advance-next-nonspace parser)) (parser-col parser)))

(define (bullet-list-bullet match)
  (match:substring match 1))

(define (ordered-list-marker parser)
  (regexp-exec re-ordered-list-marker (parser-str parser) (parser-pos parser)))

(define (ordered-list-rest parser match)
  (parser-advance parser (- (match:end match 2) (parser-col parser))))

(define (ordered-list-offset parser parser-rest)
  (- (parser-col parser-rest) (parser-col parser)))

(define (ordered-list-spaces parser)
  (- (parser-col (parser-advance-next-nonspace parser)) (parser-col parser)))

(define (ordered-list-number match)
  (string->number (match:substring match 1)))

(define (ordered-list-delimiter match)
  (match:substring match 2))

;; Link Definitions

(define (make-link-definition label destination title rest)
  (list label destination title rest))

(define (link-definition-label match)
  (first match))

(define (link-definition-destination match)
  (second match))

(define (link-definition-title match)
  (third match))

(define (link-definition-rest match)
  (fourth match))

(define (link-definition str)
  (define (link-label parser)
    (regexp-exec re-link-label (parser-str parser) (parser-pos parser)))
  (define (link-label-rest parser match)
    (parser-advance parser (- (match:end match 0) (parser-pos parser))))
  (define (link-title parser)
    (regexp-exec re-link-title (parser-str parser) (parser-pos parser)))
  (define (link-title-match-rest parser match)
    (parser-advance parser (- (match:end match 0) (parser-pos parser))))
  (define (link-title-rest title-match after-dest)
    (parser-advance-next-nonspace
     (link-title-match-rest after-dest title-match)))
  (define skip-optional-whitespace-newline
    (compose parser-advance-next-nonspace
             (cut parser-advance-optional <> #\newline)))
  (define (parser-rest-empty? parser)
    (or (parser-end? parser) (parser-char=? parser #\newline)))
  (and-let* ((parser (make-parser str))
             (label-match (link-label (parser-advance-next-nonspace parser)))
             (after-label (link-label-rest parser label-match))
             (before-dest (skip-optional-whitespace-newline after-label))
             (dest-match (link-destination (parser-advance-next-nonspace before-dest)))
             (after-dest (parser-advance-next-nonspace (cdr dest-match))))
    (let* ((title-match (link-title (parser-advance-next-nonspace
                                     (skip-optional-whitespace-newline after-dest))))
           (after-title (and title-match (link-title-rest title-match after-dest))))
      (cond
       ;; optional title must have no non-whitespace characters after title
       ((and title-match (parser-rest-empty? after-title))
        (make-link-definition (match:substring label-match 1)
                              (unescape-string (car dest-match))
                              (unescape-string (match:substring title-match 1))
                              (parser-rest-str
                               (skip-optional-whitespace-newline
                                (link-title-rest title-match after-dest)))))
       ;; must have no non-whitespace characters after destination
       ((parser-rest-empty? after-dest)
        (make-link-definition (match:substring label-match 1)
                              (unescape-string (car dest-match))
                              #f
                              (parser-rest-str
                               (skip-optional-whitespace-newline after-dest))))
       (else #f)))))

(define (link-destination-brackets parser)
  (regexp-exec re-link-destination-brackets (parser-str parser) (parser-pos parser)))

(define (link-destination-normal parser)
  (regexp-exec re-link-destination (parser-str parser) (parser-pos parser)))

(define (link-destination parser)
  (define (link-destination-rest match)
    (parser-advance parser (- (match:end match 0) (parser-pos parser))))
  (define (match:substring-suffix match)
    (cons (match:substring match 0)
          (link-destination-rest match)))
  (define (remove-brackets pair)
    (cons (substring (car pair) 1 (- (string-length (car pair)) 1))
          (cdr pair)))
  (or (and=> (link-destination-brackets parser)
             (compose remove-brackets match:substring-suffix))
      (and=> (link-destination-normal parser)
             match:substring-suffix)))






