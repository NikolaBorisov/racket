#lang scheme

(require "test.ss"
         "../collects/meta/props"
         srfi/13)

(define module-path "tmp.ss")

(define directory "../collects/redex")

(define (ss-file? path)
  (let* ([st-path (if (path? path) (path->string path) path)]
         [sp (regexp-split #rx"\\." st-path)])
    (and (>= (length sp) 2)
         (equal? (last sp) "rkt"))))

(define (run-with path)
  (with-handlers ((exn:fail? (λ (e)
                               "racket")))
    (let ([st (get-prop (substring path 3) `drdr:command-line)])
      (cond
        [(string-contains st "mzc") "mzc"]
        [(string-contains st "mred") "mred"]
        [else #f])
    )
  ))


;(map run-with (map path->string (find-files ss-file? directory)))

;; test everything
(define (build-freq)
  (with-handlers ((exn:break? (λ (x)
                                (print-freq))))
  (map (λ (x)
         (test-module x #:just-attempt #t))
       (map path->string (find-files ss-file? directory)))))

;(build-freq)

;; test particular modules
(test-module module-path)

;(test-module "../collects/framework/keybinding-lang.rkt")
;(test-module "../collects/frtime/animation/graphics.rkt")
;(test-module "../collects/preprocessor/mzpp-run.rkt")

;(test-module "../collects/html/html.rkt" #:just-attempt #t)
(print-freq)

