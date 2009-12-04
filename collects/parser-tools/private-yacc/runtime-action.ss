#lang scheme/base

;; Run-time actions
;; "parser-actions.ss" and "yacc.ss" know representation

(provide runtime-shift?
         runtime-reduce?
         runtime-accept?
         runtime-goto?

         runtime-shift-state
         runtime-reduce-prod-num
         runtime-reduce-lhs
         runtime-reduce-rhs-length
         runtime-goto-state)

;; A runtime-action is
;;  - non-negative-int        (shift)
;;  - (vector int symbol int) (reduce)
;;  - 'accept                 (accept)
;;  - negative-int            (goto)
;;  - #f                      (no-action)

(define (runtime-shift? x) (and (integer? x) (>= x 0)))
(define runtime-reduce? vector?)
(define (runtime-accept? x) (eq? x 'accept))
(define (runtime-goto? x) (and (integer? x) (< x 0)))

(define runtime-shift-state values) 
(define (runtime-reduce-prod-num x) (vector-ref x 0))
(define (runtime-reduce-lhs x) (vector-ref x 1))
(define (runtime-reduce-rhs-length x) (vector-ref x 2))
(define (runtime-goto-state x) (- (+ x 1)))
