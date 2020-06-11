#lang racket

;; variable definitions
(define WIDTH 100)
(define HEIGHT 200)

;; computed values
(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

;; constants provide a single point of control

;; local definitions are definitions placed inside function definitions
;; they only exist within the pair of parentheses in which they are defined
;; - scope of the definitions

(struct posn (x y))
(struct rectangle (width height))
(define (inside-of-rectangle? r p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define width (rectangle-width r))
  (define height (rectangle-height r))
  (and (<= 0 x) (< x width) (<= 0 y) (< y height)))

(define (winning-players lst)
  (define sorted-lst (sort lst ...))
  (define (winners lst pred)
  (cond
    [(empty? lst) (list pred)]
      [else
       (define fst (first lst))
       (if (score> (record-score pred) (record-score fst))
           (list pred)
           (cons pred (winners (rest lst) fst)))]))
  ;; START HERE:
  (winners (rest sorted-lst) (first sorted-lst)))

;; since winners shouldn't be applied to arbitrary lists and records
;; we hide it by defining it within the scope of some other function
;; that consumes a list of records