#lang scheme/base

(require (for-template scheme/base)
         syntax/boundmap
         syntax/stx
         (rename-in syntax/parse [pattern sc:pattern])
         scheme/struct-info
         "patterns.ss"
         "compiler.ss"
         "parse-helper.ss")

(provide parse-quasi)

;; parse stx as a quasi-pattern
;; parse/cert parses unquote
(define (parse-quasi stx cert parse/cert)
  (define (pq s) (parse-quasi s cert parse/cert))
  (syntax-case stx (quasiquote unquote quote unquote-splicing)
    [(unquote p) (parse/cert #'p cert)]
    [((unquote-splicing p) . rest)
     (let ([pat (parse/cert #'p cert)]
           [rpat (pq #'rest)])
       (if (null-terminated? pat)
         (append-pats pat rpat)
         (raise-syntax-error 'match "non-list pattern inside unquote-splicing"
                             stx #'p)))]
    [(p dd . rest)
     (ddk? #'dd)
     (let* ([count (ddk? #'dd)]
            [min (and (number? count) count)])
       (make-GSeq
        (parameterize ([match-...-nesting (add1 (match-...-nesting))])
          (list (list (pq #'p))))
        (list min)
        ;; no upper bound
        (list #f)
        ;; patterns in p get bound to lists
        (list #f)
        (pq #'rest)
        #f))]
    [(a . b) (make-Pair (pq #'a) (pq #'b))]
    ;; prefab structs
    [struct
     (prefab-struct-key (syntax-e #'struct))
     (let ([key (prefab-struct-key (syntax-e #'struct))]
           [pats (cdr (vector->list (struct->vector (syntax-e #'struct))))])
       (make-And (list (make-Pred #`(struct-type-make-predicate (prefab-key->struct-type '#,key #,(length pats))))
                       (make-App #'struct->vector
                                 (make-Vector (cons (make-Dummy #f) (map pq pats)))))))]
    ;; the hard cases
    [#(p ...)
     (ormap (lambda (p)
              (or (ddk? p)
                  (syntax-case p (unquote-splicing)
                    [(unquote-splicing . _) #t]
                    [_ #f])))
            (syntax->list #'(p ...)))
     (make-And (list (make-Pred #'vector?)
                     (make-App #'vector->list
                               (pq (quasisyntax/loc stx (p ...))))))]
    [#(p ...)
     (make-Vector (map pq (syntax->list #'(p ...))))]
    [bx
     (box? (syntax-e #'bx))
     (make-Box (pq (unbox (syntax-e #'bx))))]    
    [()
     (make-Null (make-Dummy #f))]
    [v
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "syntax error in quasipattern" stx))]))
