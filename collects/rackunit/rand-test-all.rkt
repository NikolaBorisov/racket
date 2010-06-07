#lang scheme

(require rackunit/rand-test-module

         "../meta/props"
         srfi/13)

(define module-path "../collects/tests/racket/contract-gen-test-code.rkt")

;(define directory "../collects/redex")
(define directory "../collects")

;(define directory "../collects/scribblings")
;(define directory "../collects/setup")
;(define directory "../collects/dynext/private")

(define (ss-file? path)
  (let* ([st-path (if (path? path) (path->string path) path)]
         [sp (regexp-split #rx"\\." st-path)])
    (and (>= (length sp) 2)
         (equal? (last sp) "rkt"))))

(define (should-test? path)
  (let* ([s-exp (with-handlers ((exn:fail? (λ (e)
                                         ;(printf "~a\n" e)
                                         '(racket *))))
                (let ([s (get-prop (substring path 3) `drdr:command-line)])
                  ;(printf "~a\n" s)
                  s))]
        [st (format "~s" s-exp)])
    (cond
      [(string-contains st "gracket") #f]
      [(string-contains st "racket") #t]
      [(string-contains st "mzscheme") #t]
      [(string-contains st "mzc") #f]
      [(string-contains st "mred") #f]
      [(equal? st "#f") #f]
      [else 
       (printf "PROP NOT HANDLED ~s\n" st) #f])
    )
  )


;; tests everything
(define (test-all
         #:just-build-freq [just-build-freq #f])
  (with-handlers ((exn:break? (λ (x)
                                (printf "Exiting...\n"))))
  (map (λ (path)
         (if (should-test? path)
             (with-handlers ((exn:fail? (λ (e)
                                          (let ([st (format "~s" e)])
                                            (if (string-contains st "test-module: files don't exist")
                                                #f
                                                (error e))))))
               (test-module path #:just-attempt just-build-freq))
             (printf "SKIP ~a\n" path)))
       (map path->string (find-files ss-file? directory))))
  null)
  

;(test-all #:just-build-freq #f)


;; test particular modules
;(test-module module-path)
(test-module "../collects/compiler/zo-parse.rkt")

;(test-module "../collects/framework/keybinding-lang.rkt")
;(test-module "../collects/frtime/animation/graphics.rkt")
;(test-module "../collects/preprocessor/mzpp-run.rkt")

;(test-module "../collects/html/html.rkt" #:just-attempt #t)
(print-freq)

