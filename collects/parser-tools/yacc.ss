#lang scheme/base
(require (for-syntax scheme/base
                     syntax/parse
                     "private-yacc/parser-builder.ss"
                     "private-yacc/grammar.ss"
                     (only-in "private-yacc/yacc-helper.ss"
                              write-yacc-output)
                     "private-yacc/parser-actions.ss"))
(require "private-lex/token.ss"
         "private-yacc/runtime-action.ss"
         mzlib/etc
         mzlib/pretty
         syntax/readerr)

(provide parser)

;; Internal debugging flag
;; (Not to be confused with debug option of parser.)
(define INTERNAL-DEBUG #f)

;; parser syntax
(define-syntax (parser stx)
  (syntax-parse stx
    [(~and
      (_ (~or (~once (~and pg:p1-grammar-clause #| ~! |#) #:name "grammar clause (p1)")
              (~once (~and pt:p1-token-clause #| ~! |#) #:name "token clause (p1)")
              (~once (~and pe:p1-end-clause #| ~! |#) #:name "end tokens clause (p1)")
              ((~datum start) . _)
              ((~datum error) . _)
              ((~datum precs) . _)
              ((~datum src-pos) . _)
              ((~datum suppress) . _)
              ((~datum debug) . _)
              ((~datum yacc-output) . _))
         ...)
      ~!
      (_ (~or (~once (~var g (grammar-clause (attribute pg.nts)
                                             (attribute pt.ts)
                                             (attribute pe.ends)))
                     #:name "grammar clause")
              (~once _:token-clause #:name "token clause")
              (~once (~var s (start-clause (attribute pg.nts)))
                     #:name "start symbols clause")
              (~once (~var e (end-clause (attribute pt.ts)))
                     #:name "end tokens clause")
              (~once err:error-clause #:name "error clause")
              (~optional (~var pr (precs-clause (attribute pt.ts)))
                         #:name "precedence clause")
              (~optional srcpos:srcpos-clause #:name "source positions clause")
              (~optional suppress:suppress-clause #:name "suppress clause")
              (~optional dbg:debug-clause #:name "debug clause")
              (~optional out:yacc-output-clause #:name "yacc-output clause"))
         ...))
     (define grammar (syntax/loc #'g (g.prod ...)))
     (define tokens (syntax->list #'(pt.group ...)))
     (define start (syntax->list #'(s.nonterminal ...)))
     (define end (syntax->list #'(e.token ...)))
     (define error #'err.handler)
     (define precs (and (attribute pr) (syntax/loc #'pr (pr.decl ...))))
     (define debug (and (attribute dbg) (syntax-e #'debug.file)))
     (define yacc-output (and (attribute out) (syntax-e #'out.file)))
     (let-values (((table all-term-syms actions check-syntax-fix)
                   (build-parser debug
                                 (and (attribute srcpos) #t)
                                 (and (attribute suppress) #t)
                                 tokens
                                 start
                                 end
                                 precs
                                 grammar)))
       (when yacc-output
         (write-yacc-output yacc-output grammar tokens (map syntax->datum start)
                            (if precs (syntax->datum precs) #f)))
       (with-syntax ((check-syntax-fix check-syntax-fix)
                     (err error)
                     (ends end)
                     (starts start)
                     (debug debug)
                     (table (convert-parse-table table))
                     (all-term-syms all-term-syms)
                     (actions actions)
                     (src-pos (and (attribute srcpos) #t)))
         #'(begin
             check-syntax-fix
             (parser-body debug err (quote starts) (quote ends)
                          table all-term-syms actions src-pos))))]))

;; The table is a vector that maps each state to a hash-table that maps a
;; terminal symbol to either an accept, shift, reduce, or goto structure.
;; We encode the structures according to the runtime-action data definition in
;; parser-actions.ss
(define (parser-body debug? err starts ends table all-term-syms actions src-pos)
  (define extract
    (if src-pos extract-src-pos extract-no-src-pos))

  (define (fix-error stack tok val start-pos end-pos get-token)
    (define (remove-input tok val start-pos end-pos)
      (if (memq tok ends)
          (raise-read-error "parser: Cannot continue after error"
                            #f #f #f #f #f)
          (let ((a (find-action stack tok val start-pos end-pos)))
            (cond [(runtime-shift? a)
                   (when INTERNAL-DEBUG
                     (printf "shift:~a~n" (runtime-shift-state a)))
                   (cons (make-stack-frame (runtime-shift-state a)
                                           val
                                           start-pos
                                           end-pos)
                         stack)]
                  [else
                   (when INTERNAL-DEBUG
                     (printf "discard input:~a~n" tok))
                   (let-values (((tok val start-pos end-pos)
                                 (extract (get-token))))
                     (remove-input tok val start-pos end-pos))]))))
    (when debug? (pretty-print stack))
    (let remove-states ()
      (let ((a (find-action stack 'error #f start-pos end-pos)))
        (cond [(runtime-shift? a)
               (when INTERNAL-DEBUG
                 (printf "shift:~a~n" (runtime-shift-state a)))
               (set! stack 
                     (cons
                      (make-stack-frame (runtime-shift-state a) 
                                        #f 
                                        start-pos
                                        end-pos)
                      stack))
               (remove-input tok val start-pos end-pos)]
              [else
               (when INTERNAL-DEBUG
                 (printf "discard state:~a~n" (car stack)))
               (cond [(< (length stack) 2)
                      (raise-read-error "parser: Cannot continue after error"
                                        #f #f #f #f #f)]
                     [else
                      (set! stack (cdr stack))
                      (remove-states)])]))))

  (define (find-action stack tok val start-pos end-pos)
    (unless (hash-ref all-term-syms tok #f)
      (if src-pos
          (err #f tok val start-pos end-pos)
          (err #f tok val))
      (raise-read-error (format "parser: got token of unknown type ~a" tok)
                        #f #f #f #f #f))
    (hash-ref (vector-ref table (stack-frame-state (car stack))) tok #f))

  (define (make-parser start-number)
    (lambda (get-token)
      (unless (and (procedure? get-token)
                   (procedure-arity-includes? get-token 0))
        (error 'get-token "expected a nullary procedure, got ~e" get-token))
      (let parsing-loop ((stack (make-empty-stack start-number))
                         (ip (get-token)))
        (let-values (((tok val start-pos end-pos)
                      (extract ip)))
          (let ((action (find-action stack tok val start-pos end-pos)))
            (cond
             [(runtime-shift? action)
              (when INTERNAL-DEBUG
                (printf "shift:~a~n" (runtime-shift-state action)))
              (parsing-loop (cons (make-stack-frame (runtime-shift-state action)
                                                    val
                                                    start-pos
                                                    end-pos)
                                  stack)
                            (get-token))]
             [(runtime-reduce? action)
              (when INTERNAL-DEBUG
                (printf "reduce:~a~n" (runtime-reduce-prod-num action)))
              (let-values (((new-stack args)
                            (reduce-stack stack 
                                          (runtime-reduce-rhs-length action)
                                          null
                                          src-pos)))
                (let ((goto 
                       (runtime-goto-state
                        (hash-ref 
                         (vector-ref table (stack-frame-state (car new-stack)))
                         (runtime-reduce-lhs action)))))
                  (parsing-loop 
                   (cons
                    (if src-pos
                        (make-stack-frame
                         goto 
                         (apply (vector-ref actions (runtime-reduce-prod-num action)) args)
                         (if (null? args) start-pos (cadr args))
                         (if (null? args) 
                             end-pos
                             (list-ref args (- (* (runtime-reduce-rhs-length action) 3) 1))))
                        (make-stack-frame
                         goto 
                         (apply (vector-ref actions (runtime-reduce-prod-num action)) args)
                         #f
                         #f))
                    new-stack)
                   ip)))]
             [(runtime-accept? action)
              (when INTERNAL-DEBUG
                (printf "accept~n"))
              (stack-frame-value (car stack))]
             [else 
              (if src-pos
                  (err #t tok val start-pos end-pos)
                  (err #t tok val))
              (parsing-loop (fix-error stack tok val start-pos end-pos get-token)
                            (get-token))]))))))
  (cond [(null? (cdr starts)) (make-parser 0)]
        [else (for/list ([_start starts] [i (in-naturals 0)])
                (make-parser i))]))


;; extract-src-pos : position-token -> symbol any any any
(define (extract-src-pos ip)
  (cond
   ((position-token? ip)
    (extract-helper (position-token-token ip)
                    (position-token-start-pos ip)
                    (position-token-end-pos ip)))
   (else
    (raise-type-error 'parser 
                      "struct:position-token"
                      0
                      ip))))

;; extract-no-src-pos : (symbol or make-token) -> symbol any any any
(define (extract-no-src-pos ip)
  (extract-helper ip #f #f))

;; extract-helper : (symbol or make-token) any any -> symbol any any any
(define (extract-helper tok v1 v2)
  (cond
   ((symbol? tok)
    (values tok #f v1 v2))
   ((token? tok)
    (values (real-token-name tok) (real-token-value tok) v1 v2))
   (else (raise-type-error 'parser 
                           "symbol or struct:token"
                           0 
                           tok))))

;; Stack

(define-struct stack-frame (state value start-pos end-pos) #:transparent)

(define (make-empty-stack i) (list (make-stack-frame i #f #f #f)))

(define (reduce-stack stack num ret-vals src-pos)
  (cond
   ((> num 0)
    (let* ((top-frame (car stack))
           (ret-vals
            (if src-pos
                (cons (stack-frame-value top-frame)
                      (cons (stack-frame-start-pos top-frame)
                            (cons (stack-frame-end-pos top-frame)
                                  ret-vals)))
                (cons (stack-frame-value top-frame) ret-vals))))
      (reduce-stack (cdr stack) (sub1 num) ret-vals src-pos)))
   (else (values stack ret-vals))))
