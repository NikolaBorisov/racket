#lang racket/base

(require "rand.rkt"
         "generator-base.rkt"
         "guts.rkt"
         "arrow.rkt"
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


(define (gen-opts have-val want-ctc have-ctc n-tests size env)
  (append (if (contract-stronger? have-ctc want-ctc)
              (list have-val)
              '())
          (if (->? have-ctc)
              (let* ([gens (map contract-struct-generator
                                (->-doms/c have-ctc))])
                (if (member #f gens)
                    '()
                    (let useful-results ([result-ctcs (->-rngs/c have-ctc)]
                                         [i 0])
                      (if (empty? result-ctcs)
                          '()
                          (let* ([args (map (λ (g)
                                              ; what should n-tests and size be
                                              (g 0 0 env))
                                            gens)])
                            (append (gen-opts (λ ()
                                                (call-with-values (λ ()
                                                                    (apply have-val args))
                                                                  (λ args
                                                                    (list-ref args i)))) 
                                              want-ctc 
                                              (first result-ctcs)
                                              n-tests
                                              size
                                              env)
                                    (useful-results (rest result-ctcs) (+ i 1))))))))
                '())))

(define (use-env n-tests size env ctc
                 #:test [is-test #f])
  (let ([options (flatten (map (λ (e-i)
                                 ;; contact-stronger? stronger weaker -> #
                                 (gen-opts (env-item-name e-i)
                                           ctc
                                           (env-item-ctc e-i)
                                           n-tests
                                           size
                                           env))
                               env))])
    (if (> (length options) 0)
        (values #t (if is-test
                       options
                       ((list-ref options (rand (length options))))))
        (values #f #f))))

(define (generator ctc env)
  (let ([g (contract-struct-generator ctc)]
        [e (let-values ([(res f) (use-env 0 0 env)])
             res)])
    (if (or g e)
        (λ (n-tests size env)
          (rand-choice
           [1/2 (g n-tests size env)]
           [else (let-values ([(res v) (use-env n-tests size env)])
                   v)]))
        #f)))





