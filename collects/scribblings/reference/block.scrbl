#lang scribble/doc
@(require "mz.ss"
          scribble/eval
          (for-label racket/block))

@(define ev (make-base-eval))
@(ev '(require racket/block))

@title[#:tag "block"]{Blocks}

@note-lib-only[racket/block]

@defform[(block defn-or-expr ...)]{

Supports a mixture of expressions and mutually recursive definitions,
as in a @scheme[module] body. 

The result of the @scheme[block] form is the result
of the last @scheme[defn-or-expr] if it is an expression,
@|void-const| otherwise. If no @scheme[defn-or-expr] is provided
(after flattening @scheme[begin] forms), the result is @|void-const|.

The final @scheme[defn-or-expr] is executed in tail position, if it is
an expression.  


@examples[#:eval ev
(define (f x)
  (block
    (define y (add1 x))
    (displayln y)
    (define z (* 2 y))
    (+ 3 z)))
(f 12)
]}
