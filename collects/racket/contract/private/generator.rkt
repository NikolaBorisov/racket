#lang racket/base

(require "rand.rkt"
         "generator-base.rkt"
         "guts.rkt"
         racket/list)

(provide
 use-env
 env-item)

;; generator 
(define-struct env-item (ctc name))

;; hash tables
;(define freq-hash (make-hash))
;(define gen-hash (make-hash))

;; thread-cell
;(define arg-names-count (make-thread-cell 0))

;; Generator integer? 
(add-generator integer?
           (λ (n-tests size env)
             (rand-choice
              [1/10 0]
              [1/10 1]
              [1/10 -1]
              [1/10 2147483647]
              [1/10 -2147483648]
              [3/10 (- 100 (rand 200))]
              [else (- 1000000000 (rand 2000000000))])))

(add-generator exact-nonnegative-integer?
               (λ (n-tests size env)
                 (abs ((find-generator integer?) n-tests size env))))


(add-generator positive?
           (λ (n-tests size env)
             (rand-choice
              [1/10 1]
              [1/10 1/3]
              [1/10 0.12]
              [1/10 2147483647]
              [else 4])))

(add-generator boolean?
           (λ (n-tests size env)
             (define (boolean?-static n-tests size env)
               (rand-choice
                [1/2 #t]
                [else #f]))
             
             (rand-choice
              [2/3 (boolean?-static n-tests size env)]
              [else (let-values ([(res v) (use-env n-tests size env boolean?)])
                      (if res
                          v
                          (boolean?-static n-tests size env)))])))

(define (use-env n-tests size env ctc)
  (let ([options (flatten (map (λ (e-i)
                                 (if (contract-stronger? ctc (env-item-ctc e-i))
                                     (list (env-item-name e-i))
                                     (list)))
                               env))])
    (if (> (length options) 0)
        (values #t (list-ref options (rand (length options))))
        (values #f #f))))





