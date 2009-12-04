#lang scheme/base
(require "grammar.ss")

;; Compile-time representations of actions

(provide (struct-out action)
         (struct-out shift)
         (except-out (struct-out reduce) make-reduce)
         (rename-out [make-reduce* make-reduce])
         (struct-out accept)
         (struct-out goto)
         (struct-out no-action)

         convert-parse-table)

;; An action is 
;;  - (make-shift int)
;;  - (make-reduce prod runtime-action)
;;  - (make-accept)
;;  - (make-goto int)
;;  - (no-action)
;; A reduce contains a runtime-reduce so that sharing of the reduces can
;; be easily transferred to sharing of runtime-reduces.

(define-struct action () #:transparent)
(define-struct (shift action) (state) #:transparent)
(define-struct (reduce action) (prod runtime-reduce) #:transparent)
(define-struct (accept action) () #:transparent)
(define-struct (goto action) (state) #:transparent)
(define-struct (no-action action) () #:transparent)

(define (make-reduce* p)
  (make-reduce p
               (vector (prod-index p)
                       (gram-sym-symbol (prod-lhs p))
                       (vector-length (prod-rhs p)))))

;; Conversion to runtime-actions
;; representation defined by "runtime-action.ss"

;; convert-parse-table : (vectorof (listof (cons/c gram-sym? action?)))
;;                    -> (vectorof (symbol runtime-action hashtable))
(define (convert-parse-table table)
  (list->vector
   (for/list ([state-entry (in-vector table)])
     (let ((ht (make-hasheq)))
       (for ([gs/action state-entry])
         (hash-set! ht
                    (gram-sym-symbol (car gs/action))
                    (action->runtime-action (cdr gs/action))))
       ht))))

(define (action->runtime-action a)
  (cond [(shift? a) (shift-state a)]
        [(reduce? a) (reduce-runtime-reduce a)]
        [(accept? a) 'accept]
        [(goto? a) (- (+ (goto-state a) 1))]
        [(no-action? a) #f]))
