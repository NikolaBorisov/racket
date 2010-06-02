#lang racket

(require schemeunit
         schemeunit/text-ui
         "contract-gen-test-code.rkt"
         racunit/rand-test-procedure
         racket/contract/private/generator)


(define (tp f)
  (test-procedure f (list) #:just-attempt #f #:exit-on-error #f #:print-error #f))

(define (tpd f)
  (test-procedure f (list)  #:just-attempt #f #:exit-on-error #t #:print-error #t))

(define (values->list f)
  (call-with-values f (λ args args)))
  


;(define contract-generator-tests
;  (test-suite
;   "Tests for the random test generator based on contracts."

(check-equal? (list #f #f)
              (list #f #f))

(check-equal? (call-with-values (λ () (use-env 0 0 (list) integer?)) (λ args args))
              (call-with-values (λ () (values #f #f)) (λ args args)))


(check-equal? (values->list (λ () (use-env 0 0 (list) integer?)))
              (list #f #f))

(check-equal? (values->list (λ () (use-env 0 
                                           0 
                                           (list (env-item (value-contract int->int/pass) 
                                                           int->int/pass)) 
                                           integer?)))
              (list #f #f))

(let ([ei (env-item (value-contract int->int/pass) 
                    int->int/pass)])
  (check-equal? (values->list (λ () (use-env 0 
                                             0 
                                             (list ei) 
                                             (value-contract int->int/fail))))
                (list #t int->int/pass)))

   ;(check-equal? (use-env 0 0 (list) integer?)
    ;             #f)
   
   (check-equal? (tp f)
                 "PASS")
   (check-equal? (tp g)
                 "FAIL")
   (check-equal? (tp f2)
                 "UNTESTABLE")
   (check-equal? (tp int->int/pass)
                 "PASS")
   (check-equal? (tp int->int/fail)
                 "FAIL")
   (check-equal? (tp pos->int/pass)
                 "PASS")
   (check-equal? (tp pos->int/fail)
                 "FAIL")
   (check-equal? (tp pos->pos/pass)
                 "PASS")
   (check-equal? (tp pos->pos/fail)
                 "FAIL")
   (check-equal? (tpd listof-int->int/pass)
                 "PASS")

   
   ;   ))

;(run-test contract-generator-tests)