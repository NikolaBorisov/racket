#lang scribble/doc
@(require "mz.ss")

@(define stx-eval (make-base-eval))
@(interaction-eval #:eval stx-eval (require (for-syntax racket/base)))

@title[#:tag "stxcmp"]{Syntax Object Bindings}

@defproc[(bound-identifier=? [a-id syntax?] [b-id syntax?]
                             [phase-level (or/c exact-integer? #f)
                                          (syntax-local-phase-level)])
         boolean?]{

Returns @scheme[#t] if the identifier @scheme[a-id] would bind
@scheme[b-id] (or vice versa) if the identifiers were substituted in a
suitable expression context at the @tech{phase level} indicated by
@scheme[phase-level], @scheme[#f] otherwise. A @scheme[#f] value for
@scheme[phase-level] corresponds to the @tech{label phase level}.

@examples[
#:eval stx-eval
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ x y)
     (if (bound-identifier=? #'x #'y)
         #'(let ([y 'wrong]) (let ([x 'binds]) y))
         #'(let ([y 'no-binds]) (let ([x 'wrong]) y)))]))
(check a a)
(check a b)
(define-syntax-rule (check-a x) (check a x))
(check-a a)
]}


@defproc[(free-identifier=? [a-id syntax?] [b-id syntax?]
                            [phase-level (or/c exact-integer? #f)
                                         (syntax-local-phase-level)])
         boolean?]{

Returns @scheme[#t] if @scheme[a-id] and @scheme[b-id] access the same
@tech{local binding}, @tech{module binding}, or @tech{top-level
binding}---perhaps via @tech{rename transformers}---at the @tech{phase
level} indicated by @scheme[phase-level]. A @scheme[#f] value for
@scheme[phase-level] corresponds to the @tech{label phase level}.

``Same module binding'' means that the identifiers refer to the same
original definition site, and not necessarily to the same
@scheme[require] or @scheme[provide] site. Due to renaming in
@scheme[require] and @scheme[provide], or due to a transformer binding
to a @tech{rename transformer}, the identifiers may return distinct
results with @scheme[syntax-e].

@examples[
#:eval stx-eval
(define-syntax (check stx)
  (syntax-case stx ()
    [(_ x)
     (if (free-identifier=? #'car #'x)
         #'(list 'same: x)
         #'(list 'different: x))]))
(check car)
(check mcar)
(let ([car list])
  (check car))
(require (rename-in racket/base [car kar]))
(check kar)
]}

@defproc[(free-transformer-identifier=? [a-id syntax?] [b-id syntax?]) boolean?]{

Same as @scheme[(free-identifier=? a-id b-id (add1 (syntax-local-phase-level)))].}

@defproc[(free-template-identifier=? [a-id syntax?] [b-id syntax?]) boolean?]{

Same as @scheme[(free-identifier=? a-id b-id (sub1 (syntax-local-phase-level)))].}

@defproc[(free-label-identifier=? [a-id syntax?] [b-id syntax?]) boolean?]{

Same as @scheme[(free-identifier=? a-id b-id #f)].}


@defproc[(check-duplicate-identifier [ids (listof identifier?)])
         (or/c identifier? #f)]{

Compares each identifier in @scheme[ids] with every other identifier
in the list with @scheme[bound-identifier=?]. If any comparison
returns @scheme[#t], one of the duplicate identifiers is returned (the
first one in @scheme[ids] that is a duplicate), otherwise the result
is @scheme[#f].}


@defproc[(identifier-binding [id-stx syntax?]
                             [phase-level (or/c exact-integer? #f)
                                          (syntax-local-phase-level)])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       (or/c 0 1)
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Returns one of three kinds of values, depending on the binding of
@scheme[id-stx] at the @tech{phase level} indicated by
@scheme[phase-level] (where a @scheme[#f] value for
@scheme[phase-level] corresponds to the @tech{label phase level}):

@itemize[ 

      @item{The result is @indexed-scheme['lexical] if @scheme[id-stx]
      has a @tech{local binding}. If @scheme['lexical] is produced for
      any @scheme[phase-level] value, then it is produced for all
      @scheme[phase-level] values.}

      @item{The result is a list of seven items when @scheme[id-stx]
      has a @tech{module binding}: @scheme[(list _source-mod _source-id
      _nominal-source-mod _nominal-source-id _source-phase _import-phase 
      _nominal-export-phase)].

        @itemize[

        @item{@scheme[_source-mod] is a module path index (see
        @secref["modpathidx"]) that indicates the defining module.}

        @item{@scheme[_source-id] is a symbol for the identifier's name
        at its definition site in the source module. This can be
        different from the local name returned by
        @scheme[syntax->datum] for several reasons: the identifier is
        renamed on import, it is renamed on export, or it is
        implicitly renamed because the identifier (or its import) was
        generated by a macro invocation.}

        @item{@scheme[_nominal-source-mod] is a module path index (see
        @secref["modpathidx"]) that indicates the module
        @scheme[require]d into the context of @scheme[id-stx] to
        provide its binding. It can be different from
        @scheme[_source-mod] due to a re-export in
        @scheme[_nominal-source-mod] of some imported identifier.  If
        the same binding is imported in multiple ways, an arbitrary
        representative is chosen.}

        @item{@scheme[_nominal-source-id] is a symbol for the
        identifier's name as exported by
        @scheme[_nominal-source-mod]. It can be different from
        @scheme[_source-id] due to a renaming @scheme[provide], even if
        @scheme[_source-mod] and @scheme[_nominal-source-mod] are the
        same.}

        @item{@scheme[_source-phase] is @scheme[1] if the source
        definition is for-syntax, @scheme[0] otherwise.}

        @item{@scheme[_import-phase] is @scheme[0] if the binding
        import of @scheme[_nominal-source-mode] is a plain
        @scheme[require], @scheme[1] if it is from a
        @scheme[for-syntax] import, etc.}

        @item{@scheme[_nominal-export-phase] is the @tech{phase level}
        of the export from @scheme[_nominal-source-mod].}

        ]}

      @item{The result is @scheme[#f] if @scheme[id-stx] has a
            @tech{top-level binding} (or, equivalently, if it is
            @tech{unbound}).}

      ]

If @scheme[id-stx] is bound to a @tech{rename-transformer}, the result
from @scheme[identifier-binding] is for the identifier in the
transformer, so that @scheme[identifier-binding] is consistent with
@scheme[free-identifier=?].}


@defproc[(identifier-transformer-binding [id-stx syntax?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       (or/c 0 1)
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Same as @scheme[(identifier-binding id-stx (add1 (syntax-local-phase-level)))].}


@defproc[(identifier-template-binding [id-stx syntax?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       (or/c 0 1)
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Same as @scheme[(identifier-binding id-stx (sub1 (syntax-local-phase-level)))].}


@defproc[(identifier-label-binding [id-stx syntax?])
         (or/c 'lexical
               #f
               (listof module-path-index?
                       symbol?
                       module-path-index?
                       symbol?
                       (or/c 0 1)
                       (or/c exact-integer? #f)
                       (or/c exact-integer? #f)))]{

Same as @scheme[(identifier-binding id-stx #f)].}

@close-eval[stx-eval]
