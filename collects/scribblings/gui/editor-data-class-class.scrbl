#lang scribble/doc
@(require "common.ss")

@defclass/title[editor-data-class% object% ()]{

An @scheme[editor-data-class%] object defines a type for
@scheme[editor-data%] objects. See also @|editordatadiscuss|.



@defconstructor[()]{

Creates a (useless) instance.

}

@defmethod[(get-classname)
           string?]{

Gets the name of the class. Names starting with @litchar{wx} are reserved for
internal use.

}

@defmethod[(read [f (is-a?/c editor-stream-in%)])
           (or/c (is-a?/c editor-data%) false/c)]{

Reads a new data object from the given stream, returning @scheme[#f] if
 there is an error.

}

@defmethod[(set-classname [v string?])
           void?]{

Sets the name of the class. Names starting with @litchar{wx} are
 reserved for internal use.

An editor data class name should usually have the form @scheme["(lib
 ...)"]  to enable on-demand loading of the class; see
 @|editordatadiscuss| for details.

}}

