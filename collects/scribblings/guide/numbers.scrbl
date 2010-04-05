#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "numbers"]{Numbers}

A Scheme @deftech{number} is either exact or inexact:

@itemize[

 @item{An @defterm{exact} number is either

       @itemize[

       @item{an arbitrarily large or small integer, such as @scheme[5],
             @scheme[99999999999999999], or @scheme[-17];}

       @item{a rational that is exactly the ratio of two arbitrarily
             small or large integers, such as @scheme[1/2],
             @scheme[99999999999999999/2], or @scheme[-3/4]; or}

       @item{a complex number with exact real and imaginary parts
             (where the imaginary part is not zero), such as @scheme[1+2i] or
             @scheme[1/2+3/4i].}

       ]}

 @item{An @defterm{inexact} number is either

       @itemize[

        @item{an IEEE floating-point representation of a number, such
              as @scheme[2.0] or @scheme[3.14e87], where the IEEE
              infinities and not-a-number are written
              @scheme[+inf.0], @scheme[-inf.0], and @scheme[+nan.0]
              (or @schemevalfont{-nan.0}); or}

        @item{a complex number with real and imaginary parts that are
              IEEE floating-point representations, such as
              @scheme[2.0+3.0i] or @scheme[-inf.0+nan.0i]; as a
              special case, an inexact complex number can have an
              exact zero real part with an inexact imaginary part.}

        ]}
]

Inexact numbers print with a decimal point or exponent specifier, and
exact numbers print as integers and fractions.  The same conventions
apply for reading number constants, but @litchar{#e} or
@litchar{#i} can prefix a number to force its parsing as an exact
or inexact number. The prefixes @litchar{#b}, @litchar{#o}, and
@litchar{#x} specify binary, octal, and hexadecimal
interpretation of digits.

@refdetails/gory["parse-number"]{the syntax of numbers}

@examples[
0.5
(eval:alts @#,schemevalfont{#e0.5} 1/2)
(eval:alts @#,schemevalfont{#x03BB} #x03BB)
]

Computations that involve an inexact number produce inexact results,
so that inexactness acts as a kind of taint on numbers. Beware,
however, that Scheme offers no ``inexact booleans'', so computations
that branch on the comparison of inexact numbers can nevertheless
produce exact results. The procedures @scheme[exact->inexact] and
@scheme[inexact->exact] convert between the two
types of numbers.

@examples[
(/ 1 2)
(/ 1 2.0)
(if (= 3.0 2.999) 1 2)
(inexact->exact 0.1)
]

Inexact results are also produced by procedures such as @scheme[sqrt],
@scheme[log], and @scheme[sin] when an exact result would require
representing real numbers that are not rational. Scheme can represent
only rational numbers and complex numbers with rational parts.

@examples[
(code:line (sin 0)   (code:comment @#,t{rational...}))
(code:line (sin 1/2) (code:comment @#,t{not rational...}))
]

In terms of performance, computations with small integers are
typically the fastest, where ``small'' means that the number fits into
one bit less than the machine's word-sized representation for signed
numbers. Computation with very large exact integers or with
non-integer exact numbers can be much more expensive than computation
with inexact numbers.

@def+int[
(define (sigma f a b)
  (if (= a b)
      0
      (+ (f a) (sigma f (+ a 1) b))))

(time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))
(time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))
]

The number categories @deftech{integer}, @deftech{rational},
@deftech{real} (always rational), and @deftech{complex} are defined in
the usual way, and are recognized by the procedures @scheme[integer?],
@scheme[rational?], @scheme[real?], and @scheme[complex?], in addition
to the generic @scheme[number?]. A few mathematical procedures accept
only real numbers, but most implement standard extensions to complex
numbers.

@examples[
(integer? 5)
(complex? 5)
(integer? 5.0)
(integer? 1+2i)
(complex? 1+2i)
(complex? 1.0+2.0i)
(abs -5)
(abs -5+2i)
(sin -5+2i)
]

The @scheme[=] procedure compares numbers for numerical equality. If
it is given both inexact and exact numbers to compare, it essentially
converts the inexact numbers to exact before comparing. The
@scheme[eqv?] (and therefore @scheme[equal?]) procedure, in contrast,
compares numbers considering both exactness and numerical equality.

@examples[
(= 1 1.0)
(eqv? 1 1.0)
]

Beware of comparisons involving inexact numbers, which by their nature
can have surprising behavior. Even apparently simple inexact numbers
may not mean what you think they mean; for example, while a base-2
IEEE floating-point number can represent @scheme[1/2] exactly, it
can only approximate @scheme[1/10]:

@examples[
(= 1/2 0.5)
(= 1/10 0.1)
(inexact->exact 0.1)
]

@refdetails["numbers"]{numbers and number procedures}