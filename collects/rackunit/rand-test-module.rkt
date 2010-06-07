#lang scheme

(require scheme/sandbox
         syntax/modresolve)

(provide
 test-module)

;; get-zo-path : path -> path
;; given the path of a module this function make a guess of where the compiled .zo file is.
(define (get-zo-path module-path)
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

;; get-env-stuff : path, path -> (list-of contracted-procedures)
;; this functions looks for all the contracted-procedures available to each function in the module.
(define (get-env-stuff module-path zo-path)
  (let* ([res-l (module-compiled-imports (parameterize ([read-accept-compiled #t])
                                         (call-with-input-file zo-path read)))]
         [res-l-0 (filter (λ (x) (= (first x) 0)) res-l)]
         [path-index-l (flatten (map (λ (x) (rest x)) res-l-0))]
         [module-paths (filter
                        path? 
                        (map (λ (x) (resolve-module-path-index x module-path)) path-index-l))]
         [zo-paths (map get-zo-path module-paths)])
    (append (get-exported-stuff module-path zo-path)
            (flatten (map (λ (m-path z-path) 
                             (get-exported-stuff m-path z-path))
                           module-paths
                           zo-paths)))))

;; get-exported-stuff : path, path -> (list-of contracted-procedures)
;; Given a path to a module and the compiled zo-file this fuction produces a list of functions that have contracts and are exported by this module
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
  
  (let ([has-contract? (dynamic-require 'racket/contract 'has-contract?)])
    
    (with-handlers ((exn:fail? (λ (x) null)))
      
      (dynamic-require module-path #f)
      
      (filter has-contract? (map (λ (x) (with-handlers ((exn:fail? (λ (x) 'idonthaveacontract)))
                                          (dynamic-require module-path x)))
                                 exported-names)))))
  
     
;; random test all the functions in a module
(define (test-module module-path 
                     #:r-seed [r-seed (random 100)]
                     #:just-attempt [just-attempt #f] 
                     #:exit-on-error [exit-on-error #t] 
                     #:only-these-fns [only-these-fns (λ (x) #t)])
  
  (let* ([zo-path (get-zo-path module-path)])
    (if (not (and (file-exists? (string-append module-path))
                  (file-exists? (string-append zo-path))))
        (error 'test-module "files don't exist ~a or ~a\n" module-path zo-path)
        
        ;              (with-handlers ((exn:fail? (λ (x) (printf "Fail testing ~a: ~a\n" module-path x)
        ;                                           #f)))
        (begin 
          (parameterize ([sandbox-output 'string])
            (let ([e (make-evaluator 'scheme/base
                                     #:allow-read (list module-path "../collects/rackunit/rand-test-module.rkt"))])
              
              (printf "Testing module ~a Seed: ~a\n" module-path r-seed)
              (call-in-sandbox-context
               e
               (λ ()
                 ((dynamic-require 'racket/contract/private/rand 'rand-seed) r-seed)))
              ;; TODO: Shuffle the procedures randomly
              (let* ([env (call-in-sandbox-context
                           e
                           (λ ()
                             (build-env 
                              (get-env-stuff module-path zo-path))
                             ))]
                     [fs (call-in-sandbox-context
                          e
                          (λ () 
                            (get-exported-stuff module-path zo-path)))]
                     [filtered-fs (filter only-these-fns fs)]
                     [results (map (λ (f)
                                     (call-in-sandbox-context
                                      e
                                      (λ ()
                                        ((dynamic-require 'rackunit/rand-test-procedure 'test-procedure)
                                         f
                                         env
                                         #:just-attempt just-attempt 
                                         #:exit-on-error exit-on-error))))
                                   filtered-fs)])
                ;(printf "~a\n" fs)
                (merge-freq (call-in-sandbox-context 
                             e
                             (λ ()
                               ((dynamic-require 'racket/contract 'get-freq)))))
                ;(printf "~a\n" (get-output e))
                (map (λ (f res)
                       (printf "~a: procedure ~a\n" res (object-name f)))
                     filtered-fs
                     results))))
          #t))))


  