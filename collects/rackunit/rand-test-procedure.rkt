#lang scheme

(provide 
 test-procedure)

;; random test a single function.
(define (test-procedure f env
                        #:just-attempt [just-attempt #f] 
                        #:exit-on-error [exit-on-error #t]
                        #:print-error [print-error #t])
  (let* ([ctc (value-contract f)]
         [tester (contract-struct-tester ctc)])
    ;(printf "~a\n" f)
    (if tester
        (if just-attempt
            "TESTABLE"
            (let ([t-res (with-handlers ([exn:fail:contract? (lambda (e) 
                                                               (if exit-on-error
                                                                   (raise e)
                                                                   (if print-error
                                                                       (printf "#######################\n~a\n" e)
                                                                       (void)))
                                                               #f)])
                           
                           (tester f 1000 1000 env))])
              (if t-res
                  "PASS"
                  "FAIL")))
        "UNTESTABLE")))
