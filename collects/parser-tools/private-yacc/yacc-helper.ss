#lang scheme/base
(require scheme/class
         "parser-actions.ss"
         "../private-lex/token-syntax.ss")
(provide write-yacc-output
         print-grammar+table-info)

(define (write-yacc-output yacc-output grammar tokens start precs)
  (with-handlers [(exn:fail:filesystem?
                   (lambda (e)
                     (fprintf 
                      (current-error-port)
                      "Cannot write yacc-output to file \"~a\"~n"
                      yacc-output)))]
    (call-with-output-file yacc-output
      (lambda (port)
        (display-yacc grammar tokens start precs port))
      #:exists 'truncate)))

(define (display-yacc grammar tokens start precs port)
  (define-syntax-rule (p arg ...) (fprintf port arg ...))
  (let* ((tokens (map syntax-local-value tokens))
         (eterms (filter e-terminals-def? tokens))
         (terms (filter terminals-def? tokens))
         (term-table (make-hasheq))
         (display-rhs
          (lambda (rhs)
            (for ([sym (car rhs)])
              (p "~a " (hash-ref term-table sym sym)))
            (when (= 3 (length rhs))
              (p "%prec ~a" (cadadr rhs)))
            (p "~n"))))
    (for ([t eterms])
      (for ([t (syntax->datum (e-terminals-def-t t))])
        (hash-set! term-table t (format "'~a'" t))))

    (for ([t terms])
      (for ([t (syntax->datum (terminals-def-t t))])
        (p "%token ~a~n" t)
        (hash-set! term-table t (format "~a" t))))

    (when precs
      (for ([prec precs])
        (p "%~a " (car prec))
        (for ([tok (cdr prec)])
          (p " ~a" (hash-ref term-table tok)))))

    (p "%start ~a~n" start)
    (p "%%~n")

    (for ([prod grammar])
      (let ((nt (car prod)))
        (p "~a: " nt)
        (display-rhs (cadr prod))
        (for ([rhs (cddr prod)])
          (p "| ")
          (display-rhs rhs))
        (p ";~n")))

    (p "%%~n")))


;; print-grammar+table-info : Grammar Table -> void
(define (print-grammar+table-info grammar table)
  (let ([num-states (vector-length table)]
        [num-gram-syms (+ (send grammar get-num-terms)
                          (send grammar get-num-non-terms))]
        [num-ht-entries (apply + (map length (vector->list table)))]
        [num-reduces
         (let ((ht (make-hasheq)))
           (for ([x (in-list (map cdr (apply append (vector->list table))))])
             (when (reduce? x)
               (hash-set! ht x #t)))
           (length (hash-map ht void)))])
    (printf "~a states, ~a grammar symbols, ~a hash-table entries, ~a reduces~n"
            num-states num-gram-syms num-ht-entries num-reduces)
    (printf "~a -- ~aKB, previously ~aKB~n"
            (/ (+ 2 num-states
                  (* 4 num-states) (* 2 1.5 num-ht-entries)
                  (* 5 num-reduces)) 256.0)
            (/ (+ 2 num-states
                  (* 4 num-states) (* 2 2.3 num-ht-entries)
                  (* 5 num-reduces)) 256.0)
            (/ (+ 2 (* num-states num-gram-syms) (* 5 num-reduces)) 256.0))))
