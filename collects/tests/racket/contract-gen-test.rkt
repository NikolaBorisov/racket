#lang scheme

(require schemeunit
         schemeunit/text-ui
         "contract-gen-test-code.rkt"
         racunit/rand-test)


(define (tp f)
  (test-procedure f  #:just-attempt #f #:exit-on-error #f #:print-error #f))

(define (tpd f)
  (test-procedure f  #:just-attempt #f #:exit-on-error #t #:print-error #t))



;(define contract-generator-tests
;  (test-suite
;   "Tests for the random test generator based on contracts."
   
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