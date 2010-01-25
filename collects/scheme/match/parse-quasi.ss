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

;; is pat a pattern representing a list?
(define (null-terminated? pat)
  (cond [(Pair? pat) (null-terminated? (Pair-d pat))]
        [(GSeq? pat) (null-terminated? (GSeq-tail pat))]
        [(Null? pat) #t]
        [else #f]))

;; combine a null-terminated pattern with another pattern to match afterwards
(define (append-pats p1 p2)
  (cond [(Pair? p1) (make-Pair (Pair-a p1) (append-pats (Pair-d p1) p2))]
        [(GSeq? p1) (make-GSeq (GSeq-headss p1)
                               (GSeq-mins p1)
                               (GSeq-maxs p1)
                               (GSeq-onces? p1)
                               (append-pats (GSeq-tail p1) p2)
                               (GSeq-mutable? p1))]
        [(Null? p1) p2]
        [else (error 'match "illegal input to append-pats")]))

(define-syntax-class (quasi-pattern cert depth)
  #:literals (quasiquote quote unquote-splicing)
  #:attributes (pat)
  #:description "quasi-pattern"
  (sc:pattern (unquote (~var p (pattern cert depth)))
              #:attr pat (attribute p.pat))
  (sc:pattern ((unquote-splicing (~var p (pattern cert depth))) . (~var rest (quasi-pattern cert depth)))
              #:fail-unless (null-terminated? (attribute p.pat))
              "non-list pattern inside `unquote-splicing'"
              #:attr pat (append-pats (attribute p.pat) (attribute rest.pat)))
  (sc:pattern ((~var p (quasi-pattern cert (add1 depth))) dd:ddk . (~var rest (quasi-pattern cert depth)))
              #:attr pat (make-GSeq (list (list (attribute p.pat))) 
                                    (list (attribute dd.min))
                                    ;; no upper bound
                                    (list #f)
                                    ;; patterns in p get bound to lists
                                    (list #f)
                                    (attribute rest.pat)
                                    #f))
  (sc:pattern ((~var qp1 (quasi-pattern cert depth)) . (~var qp2 (quasi-pattern cert depth)))
              #:attr pat (make-Pair (attribute qp1.pat) (attribute qp2.pat)))
  (sc:pattern struct
              #:attr key (prefab-struct-key (syntax-e #'struct))
              #:when (attribute key)
              #:with ((~var qp (quasi-pattern cert depth)) ...) 
              (cdr (vector->list (struct->vector (syntax-e #'struct))))
              #:attr pat (make-And (list 
                                    (make-Pred 
                                     #`(struct-type-make-predicate
                                        (prefab-key->struct-type
                                         '#,(attribute key) 
                                         #,(length (syntax->list #'(qp ...))))))
                                    (make-App 
                                     #'struct->vector
                                     (make-Vector 
                                      (cons (make-Dummy #f) 
                                            (attribute qp.pat)))))))
  (sc:pattern #((~var qp (quasi-pattern cert depth)) ...)
              #:attr pat (make-Vector (attribute qp.pat)))
  (sc:pattern () #:attr pat (make-Null (make-Dummy #f)))
  (sc:pattern #&(~var qp (quasi-pattern cert depth))
              #:attr pat (make-Box (attribute qp.pat)))
  (sc:pattern lit:literal-pattern
              #:attr pat (attribute lit.pat)))

(define-syntax-class (pattern cert depth)
  #:attributes (pat)
  (sc:pattern (list)
              #:attr pat #f))

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
