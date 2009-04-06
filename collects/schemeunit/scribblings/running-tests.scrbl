#lang scribble/doc
@(require "base.ss")

@title[#:tag "running"]{Programmatically Running Tests and Inspecting Results}

SchemeUnit provides an API for running tests, from which
custom UIs can be created.

@section{Result Types}

@defstruct[(exn:test exn) ()]{

The base structure for SchemeUnit exceptions.  You should
never catch instances of this type, only the subtypes
documented below.}

@defstruct[(exn:test:check exn:test) ([stack (listof check-info)])]{

A @scheme[exn:test:check] is raised when an check fails, and
contains the contents of the check-info stack at the
time of failure.}

@defstruct[test-result ([test-case-name (or/c string #f)])]{

A test-result is the result of running the test with
the given name (with @scheme[#f] indicating no name is available).}

@defstruct[(test-failure test-result) ([result any])]{

Subtype of test-result representing a test failure.}

@defstruct[(test-error test-result) ([result exn])]{

Subtype of test-result representing a test error.}

@defstruct[(test-success test-result) ([result any])]{

Subtype of test-result representing a test success.}


@section{Functions to Run Tests}

@defproc[(run-test-case (name (or/c string #f)) (action (-> any)))
         test-result]{

Runs the given test case, returning a result representing success, failure, or error.}


@defproc[(run-test (test (or/c test-case? test-suite?)))
         (R = (listof (or/c test-result R)))]{

Runs the given test (test case or test suite) returning a
tree (list of lists) of results}

Example:

@schemeblock[
  (run-test
     (test-suite
      "Dummy"
      (test-case "Dummy" (check-equal? 1 2))))
]

@defproc[(fold-test-results [result-fn ('b 'c ... 'a . -> . 'a)]
                            [seed 'a]
			    [test (or/c test-case? test-suite?)]
			    [#:run run (string (() -> any) . -> . 'b 'c ...)]
 			    [#:fdown fdown (string 'a . -> . 'a)] 
			    [#:fup fup (string 'a . -> . 'a)])
          'a]{

Fold @scheme[result-fn] pre-order left-to-right depth-first
over the results of @scheme[run].  By default @scheme[run]
is @scheme[run-test-case] and @scheme[fdown] and
@scheme[fup] just return the seed, so @scheme[result-fn] is
folded over the test results.

This function is useful for writing custom folds (and hence
UIs) over test results without you having to take care of
all the expected setup and teardown.  For example,
@scheme[fold-test-results] will run test suite before and
after actions for you.  However it is still flexible enough,
via its keyword arguments, to do almost anything that foldts
can.  Hence it should be used in preference to foldts.

@scheme[result-fn] is a function from the results of
@scheme[run] (defaults to a @scheme[test-result]) and the
seed to a new seed

Seed is any value

Test is a test-case or test-suite

Run is a function from a test case name (string) and action
(thunk) to any values.

FDown is a function from a test suite name (string) and the
seed, to a new seed

FUp is a function from a test suite name (string) and the
seed, to a new seed.}

Examples:

The following code counts the number of successes

@schemeblock[
(define (count-successes test)
  (fold-test-results
   (lambda (result seed)
     (if (test-success? result)
         (add1 seed)
         seed))
   0
   test))]

The following code returns the symbol @scheme['burp] instead
of running test cases.  Note how the result-fn receives the
value of run.

@schemeblock[
(define (burp test)
  (fold-test-results
   (lambda (result seed) (cons result seed))
   null
   test
   #:run (lambda (name action) 'burp)))]


@defproc[(foldts [fdown (test-suite string thunk thunk 'a -> 'a)]
		 [fup (test-suite string thunk thunk 'a 'a -> 'a)]
		 [fhere(test-case string thunk 'a -> 'a)]
		 [seed 'a]
		 [test (or/c test-case? test-suite?)])
    		 'a]{

Foldts is a nifty tree fold (created by Oleg Kiselyov) that
folds over a test in a useful way (fold-test-results isn't
that useful as you can't specify actions around test cases).

Fdown is a function of test suite, test suite name, before
action, after action, and the seed.  It is run when a test
suite is encountered on the way down the tree (pre-order).

Fup is a function of test suite, test suite name, before
action, after action, the seed at the current level, and the
seed returned by the children.  It is run on the way up the
tree (post-order).

Fhere is a function of the test case, test case name, the
test case action, and the seed. (Note that this might change
in the near future to just the test case.  This change would
be to allow fhere to discriminate subtypes of test-case,
which in turn would allow test cases that are, for example,
ignored).}

Example:

Here's the implementation of fold-test-results in terms of
foldts:

@schemeblock[
(define (fold-test-results suite-fn case-fn seed test)
  (foldts
   (lambda (suite name before after seed)
     (before)
     (suite-fn name seed))
   (lambda (suite name before after seed kid-seed)
     (after)
     kid-seed)
   (lambda (case name action seed)
     (case-fn
      (run-test-case name action)
      seed))
   seed
   test))
]

If you're used to folds you'll probably be a bit surprised
that the functions you pass to foldts receive both the
structure they operate on, and the contents of that
structure.  This is indeed unusual.  It is done to allow
subtypes of test-case and test-suite to be run in customised
ways.  For example, you might define subtypes of test case
that are ignored (not run), or have their execution time
recorded, and so on.  To do so the functions that run the
test cases need to know what type the test case has, and
hence is is necessary to provide this information.

If you've made it this far you truly are a master SchemeUnit
hacker.  As a bonus prize we'll just mention that the code
in hash-monad.ss and monad.ss might be of interest for
constructing user interfaces.  The API is still in flux, so
isn't documented here.  However, do look at the
implementation of @scheme[run-tests] for examples of use.