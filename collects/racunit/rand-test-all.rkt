#lang scheme

(require racunit/rand-test
;         meta/props
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
  (let ([st (with-handlers ((exn:fail? (λ (e)
                                         "racket")))
                (get-prop (substring path 3) `drdr:command-line))])
    (cond
      [(string-contains st "racket") #t]
      [(string-contains st "mzscheme") #t]
      [(string-contains st "mzc") #f]
      [(string-contains st "mred") #f]
      [(equal? st "") #f]
      [else 
       (printf "PROP NOT HANDLED ~s\n" st) #f])
    )
  )

#|
(define (should-test? path)
  (let ([ev (run-with path)])
    (or (equal? ev "racket")
        (equal? ev "mzscheme"))))
|#
;(map run-with (map path->string (find-files ss-file? directory)))

;; test everything
(define (build-freq)
  (with-handlers ((exn:break? (λ (x)
                                (printf "Exiting...\n"))))
  (map (λ (path)
         (if (should-test? path)
             (test-module path #:just-attempt #t)
             (printf "SKIP ~a\n" path)))
       (map path->string (find-files ss-file? directory))))
  null)

;(build-freq)


;; test particular modules
(test-module module-path)

;(test-module "../collects/framework/keybinding-lang.rkt")
;(test-module "../collects/frtime/animation/graphics.rkt")
;(test-module "../collects/preprocessor/mzpp-run.rkt")

;(test-module "../collects/html/html.rkt" #:just-attempt #t)
;(print-freq)

