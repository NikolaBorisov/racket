#lang scheme/base
(require (for-syntax scheme/base))

(provide set seteq seteqv
         set? set-eq? set-eqv? set-equal?
         set-empty? set-count
         set-member? set-add set-remove
         set-union set-intersect set-subtract
         set-map set-for-each 
         (rename-out [*in-set in-set]))

(define-struct set (ht)
  #:omit-define-syntaxes
  #:property prop:custom-write
  (lambda (s port write?)
    (define print (if write? write display))
    (write-string "#<set:" port)
    (set-for-each s 
                  (lambda (e) 
                    (write-string " " port)
                    (print e port)))
    (write-string ">" port))
  #:property prop:equal+hash (list 
                              (lambda (set1 set2 =?)
                                (=? (set-ht set1) (set-ht set2)))
                              (lambda (set hc) (add1 (hc (set-ht set))))
                              (lambda (set hc) (add1 (hc (set-ht set)))))
  #:property prop:sequence (lambda (v) (*in-set v)))

(define (set . elems)
  (make-set (make-immutable-hash (map (lambda (k) (cons k #t)) elems))))
(define (seteq . elems)
  (make-set (make-immutable-hasheq (map (lambda (k) (cons k #t)) elems))))
(define (seteqv . elems)
  (make-set (make-immutable-hasheqv (map (lambda (k) (cons k #t)) elems))))

(define (set-eq? set)
  (unless (set? set) (raise-type-error 'set-eq? "set" 0 set))
  (hash-eq? (set-ht set)))
(define (set-eqv? set)
  (unless (set? set) (raise-type-error 'set-eqv? "set" 0 set))
  (hash-eqv? (set-ht set)))
(define (set-equal? set)
  (unless (set? set) (raise-type-error 'set-equal? "set" 0 set))
  (let* ([ht (set-ht set)])
    (not (or (hash-eq? ht)
             (hash-eqv? ht)))))

(define (set-empty? set)
  (unless (set? set) (raise-type-error 'set-empty? "set" 0 set))
  (zero? (hash-count (set-ht set))))

(define (set-count set)
  (unless (set? set) (raise-type-error 'set-count "set" 0 set))
  (hash-count (set-ht set)))

(define (set-member? set v)
  (unless (set? set) (raise-type-error 'set-member? "set" 0 set v))
  (hash-ref (set-ht set) v #f))

(define (set-add set v)
  (unless (set? set) (raise-type-error 'set-add "set" 0 set v))
  (make-set (hash-set (set-ht set) v #t)))

(define (set-remove set v)
  (unless (set? set) (raise-type-error 'set-remove "set" 0 set v))
  (make-set (hash-remove (set-ht set) v)))

(define set-union
  (case-lambda
   [(set) 
    (unless (set? set) (raise-type-error 'set-union "set" 0 set))
    set]
   [(set set2)
    (unless (set? set) (raise-type-error 'set-union "set" 0 set set2))
    (unless (set? set2) (raise-type-error 'set-union "set" 1 set set2))
    (let ([ht (set-ht set)]
          [ht2 (set-ht set2)])
      (unless (and (eq? (hash-eq? ht) (hash-eq? ht2))
                   (eq? (hash-eqv? ht) (hash-eqv? ht2)))
        (raise-mismatch-error 'set-union "set's equivalence predicate is not the same as the first set: "
                              set2))
      (let-values ([(ht ht2)
                    (if ((hash-count ht2) . > . (hash-count ht))
                        (values ht2 ht)
                        (values ht ht2))])
        (make-set
         (for/fold ([ht ht]) ([v (in-hash-keys ht2)])
           (hash-set ht v #t)))))]
   [(set . sets)
    (for ([s (in-list (cons set sets))]
          [i (in-naturals)])
      (unless (set? s) (apply raise-type-error 'set-union "set" i sets)))
    (for/fold ([set set]) ([set2 (in-list sets)])
      (set-union set set2))]))

(define (empty-like ht)
  (cond
   [(hash-eqv? ht) #hasheqv()]
   [(hash-eq? ht) #hasheq()]
   [else #hash()]))

(define set-intersect
  (case-lambda
   [(set) 
    (unless (set? set) (raise-type-error 'set-intersect "set" 0 set))
    set]
   [(set set2)
    (unless (set? set) (raise-type-error 'set-intersect "set" 0 set set2))
    (unless (set? set2) (raise-type-error 'set-intersect "set" 1 set set2))
    (let ([ht1 (set-ht set)]
          [ht2 (set-ht set2)])
      (unless (and (eq? (hash-eq? ht1) (hash-eq? ht2))
                   (eq? (hash-eqv? ht1) (hash-eqv? ht2)))
        (raise-mismatch-error 'set-union "set's equivalence predicate is not the same as the first set: "
                              set2))
      (let-values ([(ht1 ht2) (if ((hash-count ht1) . < . (hash-count ht2))
                                  (values ht1 ht2)
                                  (values ht2 ht1))])
        (make-set
         (for/fold ([ht (empty-like (set-ht set))]) ([v (in-hash-keys ht1)])
           (if (hash-ref ht2 v #f)
               (hash-set ht v #t)
               ht)))))]
   [(set . sets)
    (for ([s (in-list (cons set sets))]
          [i (in-naturals)])
      (unless (set? s) (apply raise-type-error 'set-intersect "set" i sets)))
    (for/fold ([set set]) ([set2 (in-list sets)])
      (set-intersect set set2))]))

(define set-subtract
  (case-lambda
   [(set) 
    (unless (set? set) (raise-type-error 'set-subtract "set" 0 set))
    set]
   [(set set2)
    (unless (set? set) (raise-type-error 'set-subtract "set" 0 set set2))
    (unless (set? set2) (raise-type-error 'set-subtract "set" 1 set set2))
    (let ([ht1 (set-ht set)]
          [ht2 (set-ht set2)])
      (unless (and (eq? (hash-eq? ht1) (hash-eq? ht2))
                   (eq? (hash-eqv? ht1) (hash-eqv? ht2)))
        (raise-mismatch-error 'set-union "set's equivalence predicate is not the same as the first set: "
                              set2))
      (if ((* 2 (hash-count ht1)) . < . (hash-count ht2))
          ;; Add elements from ht1 that are not in ht2:
          (make-set
           (for/fold ([ht (empty-like ht1)]) ([v (in-hash-keys ht1)])
             (if (hash-ref ht2 v #f)
                 ht
                 (hash-set ht v #t))))
          ;; Remove elements from ht1 that are in ht2
          (make-set
           (for/fold ([ht ht1]) ([v (in-hash-keys ht2)])
             (hash-remove ht v)))))]
   [(set . sets)
    (for ([s (in-list (cons set sets))]
          [i (in-naturals)])
      (unless (set? s) (apply raise-type-error 'set-subtract "set" i sets)))
    (for/fold ([set set]) ([set2 (in-list sets)])
      (set-subtract set set2))]))

(define (set-map set proc)
  (unless (set? set) (raise-type-error 'set-map "set" 0 set proc))
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-type-error 'set-map "procedure (arity 1)" 1 set proc))
  (for/list ([v (in-set set)])
    (proc v)))

(define (set-for-each set proc)
  (unless (set? set) (raise-type-error 'set-for-each "set" 0 set proc))
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 1))
    (raise-type-error 'set-for-each "procedure (arity 1)" 1 set proc))
  (for ([v (in-set set)])
    (proc v)))

(define (in-set set)
  (unless (set? set) (raise-type-error 'in-set "set" 0 set))
  (in-hash-keys (set-ht set)))

(define-sequence-syntax *in-set
  (lambda () #'in-set)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ st)]
       #`[(id)
          (:do-in
           ;; outer bindings:
           ([(ht) (let ([s st]) (if (set? s) (set-ht s) (list s)))])
           ;; outer check:
           (unless (hash? ht)
             ;; let `in-set' report the error:
             (in-set (car ht)))
           ;; loop bindings:
           ([pos (hash-iterate-first ht)])
           ;; pos check
           pos
           ;; inner bindings
           ([(id) (hash-iterate-key ht pos)])
           ;; pre guard
           #t
           ;; post guard
           #t
           ;; loop args
           ((hash-iterate-next ht pos)))]])))