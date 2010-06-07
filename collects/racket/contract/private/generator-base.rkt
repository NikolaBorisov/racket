#lang racket/base

(provide
 find-generator
 add-generator
 
 print-freq
 get-freq
 merge-freq
 count-missing-generator
 
 get-arg-names-space
 gen-arg-names
 env-item)

 
;; generator 
(define-struct env-item (ctc name))

;; hash tables
(define freq-hash (make-hash))
(define gen-hash (make-hash))

;; thread-cell
(define arg-names-count (make-thread-cell 0))

;; given a predicate returns a generator for this predicate or #f
(define (find-generator func [name "internal"])
  (let ([gen (hash-ref gen-hash func #f)])
    (if gen
        gen
        (begin 
;          (printf "func ~a\n" name)
          (count-missing-generator name)
          #f))))

(define (add-generator ctc gen)
  (hash-set! gen-hash ctc gen))
  

(define (get-arg-names-space space-needed)
  (let ([rv (thread-cell-ref arg-names-count)])
    (thread-cell-set! arg-names-count (+ rv space-needed))
    rv))

(define (gen-arg-names st-num size)
  (cond
    [(<= size 0) (list)]
    [else (cons (string->symbol (string-append "x-" (number->string st-num)))
                (gen-arg-names (+ st-num 1) (- size 1)))]))

(define (print-freq)
  (printf "Generator frequency:\n")
  (let* ([l (hash-map freq-hash (λ (k v)
                                 (list k v)))]
         [l-s (sort l (λ (e1 e2)
                        (> (list-ref e1 1)
                           (list-ref e2 1))))])
    (map (λ (x)
           (printf "# ~a : ~a\n" 
                   (list-ref x 1)
                   (list-ref x 0)))
         l-s))
  null)

(define (count-missing-generator ctc)
  (hash-update! freq-hash 
               ctc 
               (λ (x)
                 (+ x 1))
               0))

    

(define (get-freq)
  freq-hash)

(define (merge-freq h)
  (hash-for-each h (λ (k v)
                     (hash-set! freq-hash k (+ (hash-ref freq-hash k 0)
                                               v)))))
