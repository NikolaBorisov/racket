#lang scheme/base
(provide wrong-syntax
         current-syntax-context)

(define current-syntax-context (make-parameter #f))

(define (wrong-syntax stx format-string . args)
  (unless (or (eq? stx #f) (syntax? stx))
    (raise-type-error 'wrong-syntax "syntax or #f" 0 (list* stx format-string args)))
  (let* ([ctx (current-syntax-context)]
         [blame (syntax-property ctx 'report-errors-as)])
    (raise-syntax-error (if (symbol? blame) blame #f)
                        (apply format format-string args)
                        ctx
                        stx)))