#lang scribble/doc
@(require scribble/manual
	  (for-label scheme/base
                     slatex/slatex-wrapper))

@(define latex @exec{latex})
@(define pdflatex @exec{pdf-latex})
@(define slatex @exec{slatex})

@title{SLaTeX Wrapper}

@defmodule[slatex/slatex-wrapper]

To use SLaTeX as a standalone program, either drag your
@filepath{.tex} file onto SLaTeX (under Windows or MacOS X), or type
@exec{slatex file} in a command shell.

In addition to the SLaTeX tools, this collection contains a parallel
set of PDF-SLaTeX tools, which are identical except that they
call @pdflatex rather than @|latex|.  In particular, there is now a
launcher called (PDF-SLaTeX/pdf-slatex).

@deftogether[(
  @defproc[(slatex (filename string?)) boolean?]
  @defproc[(pdf-slatex (filename string?)) boolean?])]{

Accepts a string naming a file and runs @slatex and @latex on the
file. It calls @scheme[filename->latex-filename] on @scheme[filename].

@scheme[pdf-slatex] is like @scheme[slatex] except that it calls
@pdflatex rather than @latex, and produces PDF output instead of PS
output.}

@defproc[(slatex/no-latex (filename string?)) void?]{

Runs @slatex on the file named by @scheme[filename], without calling
@|latex|. That is, it only processes the @filepath{.tex} file to
produce the @filepath{.Z} files.  It calls
@scheme[filename->latex-filename] on @scheme[filename].}

@deftogether[(
  @defproc[(latex (filename string?)) boolean?]
  @defproc[(pdf-latex (filename string?)) boolean?])]{

Runs @latex on the file named by @scheme[filename]. It calls
@scheme[filename->latex-filename] on @scheme[filename].

@scheme[pdf-latex] is like @scheme[latex] except that it calls
@pdflatex rather than @latex, and produces PDF output instead of PS
output.}

@defproc[(filename->latex-filename (filename string?)) string?]{

Accepts a filename and, if that file exists, it returns it. If the
filename appended with the suffix @filepath{.tex} exists, that
filename is returned. Otherwise, an exception is raised.}

@emph{NOTE:} in order to run @slatex, your @envvar{TEXINPUTS}
environment variable must contain a reference to the directory in
which the most recent version of @filepath{slatex.sty} lives.  That
file resides by default in the @filepath{slatex} collection of the
main installation.
