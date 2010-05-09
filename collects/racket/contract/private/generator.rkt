#lang scheme/base

(require "rand.rkt")

(provide
 env-item
 find-generator
 get-arg-names-space
 gen-arg-names
 print-freq
 count-missing-generator
 (all-from-out "rand.rkt"))

;; generator 
(define-struct env-item (ctc name))

;; hash tables
(define freq-hash (make-hash))
(define gen-hash (make-hash))

;; thread-cell
(define arg-names-count (make-thread-cell 0))

;; Generator integer? 
(hash-set! gen-hash integer? 
           (λ (n-tests size env)
             (rand-choice
              [1/10 0]
              [1/10 1]
              [1/10 -1]
              [1/10 2147483647]
              [1/10 -2147483648]
              [3/10 (- 100 (rand 200))]
              [else (- 1000000000 (rand 2000000000))])))


(hash-set! gen-hash positive?
           (λ (n-tests size env)
             (rand-choice
              [1/10 1]
              [1/10 1/3]
              [1/10 0.12]
              [1/10 2147483647]
              [else 4])))
;              [else (* (rand 1) 2000000000)])))
;             (+ (abs (- 50 (rand 100))) 1)))

(define (count-missing-generator ctc)
  (hash-update! freq-hash 
               ctc 
               (λ (x)
                 (+ x 1))
               0))

;; given a predicate returns a generator for this predicate or #f
(define (find-generator func name)
  (let ([gen (hash-ref gen-hash func #f)])
    (if gen
        gen
        (begin 
;          (printf "func ~a\n" name)
          (count-missing-generator name)
          #f))))

(define (print-freq)
  (printf "Generator frequency:\n")
  (hash-for-each freq-hash (λ (k v)
                             (printf "~a ~a\n" v k))))

(define (get-arg-names-space space-needed)
  (let ([rv (thread-cell-ref arg-names-count)])
    (thread-cell-set! arg-names-count (+ rv space-needed))
    rv))

(define (gen-arg-names st-num size)
  (cond
    [(<= size 0) (list)]
    [else (cons (string->symbol (string-append "x-" (number->string st-num)))
                (gen-arg-names (+ st-num 1) (- size 1)))]))

