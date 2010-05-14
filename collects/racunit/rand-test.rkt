#lang scheme

(provide
 test-module
 test-procedure)

;(define has-contract? (λ (f) #t))
;(require contract)

;(define module-path "tmp.ss")
;(define zo-file "compiled/tmp_ss.zo")



;(define module-path "../collects/html/html.ss")
;(define zo-file "../collects/html/compiled/html_ss.zo")

(define (get-exported-stuff module-path zo-file)

 (define (get-names table)
   (if table
       (map car (cdr table))
       '()))

 (define exported-names
   (let-values ([(vars macros)
                 (module-compiled-exports
                  (parameterize ([read-accept-compiled #t])
                    (call-with-input-file zo-file read)))])
     (append (get-names (assoc 0 macros))
             (get-names (assoc 0 vars)))))
  (printf "before\n")
  (with-handlers ((exn:fail? (λ (x) (printf "dynamic-require module error: ~a\n" x)
                               null)))
    
    (dynamic-require module-path #f)
    (printf "after\n")
    (filter has-contract? (map (λ (x) (with-handlers ((exn:fail? (λ (x) (printf "DRE: ~a\n" x)
                                                                 'idonthaveacontract)))
                                      (dynamic-require module-path x)))
                             exported-names))

  ))
  ;(printf "after\n")
  

;(get-exported-stuff "tmp.ss" "compiled/tmp_ss.zo")
;(get-exported-stuff module-path zo-file)

;; random test a single function.
(define (test-procedure f 
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
                           (tester f 1000 1000 (list)))])
              (if t-res
                  "PASS"
                  "FAIL")))
        "UNTESTABLE")))
     
;; random test all the functions in a module
(define (test-module module-path 
                     #:r-seed [r-seed (random 100)]
                     #:just-attempt [just-attempt #f] 
                     #:exit-on-error [exit-on-error #t] 
                     #:only-these-fns [only-these-fns (λ (x) #t)])
  (define (compute-zo-path module-path)
    (let* ([path (regexp-split #rx"/" module-path)]
           [pure-path (remove (list-ref path (- (length path) 1)) path)]
           [file-and-ext (regexp-split #rx"\\." (last path))]
           [zo-filename (string-append (first file-and-ext) "_" (second file-and-ext) ".zo")]
           [zo-path (string-append (string-join pure-path "/") 
                                   (if (> (length pure-path) 0) 
                                       "/" 
                                       "")
                                   "compiled/" 
                                   zo-filename)])
      zo-path))
  
  (printf "Testing module ~a Seed: ~a\n" module-path r-seed)
  (rand-seed r-seed)
  (parameterize ([current-input-port (open-input-bytes #"")]
                 #;[current-namespace (make-base-namespace)])
  (let* ([zo-path (compute-zo-path module-path)])
    (if (not (and (file-exists? module-path)
                  (file-exists? zo-path)))
        (printf "files don't exist ~a or ~a\n" module-path zo-path)
        ;; TODO: Shuffle the procedures randomly
        (let* ([fs (get-exported-stuff module-path zo-path)]
               [filtered-fs (filter only-these-fns fs)]
               [results (map (λ (f)
                 (test-procedure f 
                                 #:just-attempt just-attempt 
                                 #:exit-on-error exit-on-error)) filtered-fs)])
          (map (λ (f res)
                  (printf "~a: procedure ~a\n" res f))
                filtered-fs
                results)))
    #t)))


;(test-module module-path)
  