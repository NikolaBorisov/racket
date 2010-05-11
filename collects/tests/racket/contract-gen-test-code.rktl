#lang scheme


(define (int->int/pass x)
  (+ x (random 100)))

(define (int->int/fail x)
  (+ x 1/2))

(define (pos->int/pass x)
  (round x))

(define (pos->int/fail x)
  4.2)

(define (pos->pos/pass x)
  (+ x 10))

(define (pos->pos/fail x)
  (- x 1))

(define (f fun)
  (fun -843))

(define (g i)
  (λ (x)
    (+ (abs x) -100)))

(define (f2 n)
  n)

(define (listof-int->int/pass l)
  (length l))

(provide/contract
 [int->int/pass (integer? . -> . integer?)]
 [int->int/fail (integer? . -> . integer?)]
 [pos->int/pass (positive? . -> . integer?)]
 [pos->int/fail (positive? . -> . integer?)]
 [pos->pos/pass (positive? . -> . positive?)]
 [pos->pos/fail (positive? . -> . positive?)]
 [listof-int->int/pass ((listof integer?) . -> . integer?)]
 [f ((integer? . -> . positive?) . -> . positive?)]
 [g (integer? . -> . (integer? . -> . positive?))]
 [f2 (negative? . -> . negative?)])