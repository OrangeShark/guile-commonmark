;; Copyright (C) 2016 Erik Edrosa <erik.edrosa@gmail.com>
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
  #:use-module (srfi srfi-9)
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
            code-block
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
  (substring (parser-str parser) (parser-pos parser)))

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
  (string-match (string-append "^" fence "$") (parser-str parser) (parser-pos parser)))

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

(define (link-definition str)
  (regexp-exec re-link-definition str))

(define (link-definition-rest match)
  (match:suffix match))

(define (link-definition-label match)
  (match:substring match 1))

(define (link-definition-destination match)
  (match:substring match 3))

(define (link-definition-title match)
  (match:substring match 7))
