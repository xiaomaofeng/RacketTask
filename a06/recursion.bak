#lang racket
;;(for/sum ([x (in-range 10 15)]) x)
  
;(in-range 10 15)
;(spread 1 15)
;(sort (list 4 1 2 3) < )
;(define (spread proc xs)
;  (if (null? xs)
;      0
;      (proc (- (first xs) (rest xs))
(define (ls-max L)
  (cond
    [(empty? L) #f]
    [(empty? (rest L)) (first L)]
    [(>= (first L) (ls-max (rest L))) (first L)]
    [else (ls-max (rest L))]))

(ls-max '(2 3 1))