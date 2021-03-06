#lang scribble/doc
@(require "common.ss")

@title{System Menus}


@defproc[(current-eventspace-has-standard-menus?)
         boolean?]{
Returns @scheme[#t] for Mac OS X when the current eventspace is the
 initial one, since that eventspace is the target for the standard
 application menus. For any other system or eventspace, the result is
 @scheme[#f].

This procedure is intended for use in deciding whether to include a
 @onscreen{Quit}, @onscreen{About}, and @onscreen{Preferences} menu
 item in a frame's menu. Under Mac OS X, the application
 @onscreen{Quit} menu triggers a call to a frame's
@method[top-level-window<%> on-exit] method, the @onscreen{About} menu item is controlled by
 @scheme[application-about-handler], and the
 @onscreen{Preferences} menu item is controlled by
 @scheme[application-preferences-handler].

}

@defproc[(current-eventspace-has-menu-root?)
         boolean?]{
Returns @scheme[#t] for Mac OS X when the current eventspace is the
 initial one, since that eventspace can supply a menu bar to be active
 when no frame is visible. For any other system or eventspace, the
 result is @scheme[#f].

This procedure is intended for use in deciding whether to create a
 @scheme[menu-bar%] instance with @scheme['root] as its parent.

}

@defproc*[([(application-about-handler)
            (-> any)]
           [(application-about-handler [handler-thunk (-> any)])
            void?])]{

When the current eventspace is the initial eventspace, this
procedure retrieves or installs a thunk that is called when the
user selects the application @onscreen{About} menu item in Mac OS
X.  The thunk is always called in the initial eventspace's
handler thread (as a callback).

The default handler displays a generic Racket dialog.

If the current eventspace is not the initial eventspace, this
procedure returns @scheme[void] (when called with zero arguments)
or has no effect (when called with a handler).

}

@defproc*[([(application-preferences-handler)
            (or/c (-> any) false/c)]
           [(application-preferences-handler [handler-thunk (or/c (-> any) false/c)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a thunk that is called when the user selects
 the application @onscreen{Preferences} menu item in Mac OS X.  The
 thunk is always called in the initial eventspace's handler thread (as
 a callback). If the handler is set to @scheme[#f], the
 @onscreen{Preferences} item is disabled.

The default handler is @scheme[#f].

If the current eventspace is not the initial eventspace, this
procedure returns @scheme[void] (when called with zero arguments)
or has no effect (when called with a handler).
}

@defproc*[([(application-quit-handler)
            (-> any)]
           [(application-quit-handler [handler-thunk (-> any)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a thunk that is called when the user requests
 that the application quit (e.g., through the @onscreen{Quit} menu
 item in Mac OS X, or when shutting down the machine in Windows). The
 thunk is always called in the initial eventspace's handler thread (as
 a callback). If the result of the thunk is @scheme[#f], then the
 operating system is explicitly notified that the application does not
 intend to quit (under Windows).

The default handler queues a call to the
 @method[top-level-window<%> can-exit?] method of the most
 recently active frame in the initial eventspace (and then calls the
 frame's @method[top-level-window<%> on-exit] method if the
 result is true). The result is @scheme[#t] if the eventspace is
 left with no open frames after
 @method[top-level-window<%> on-exit] returns, @scheme[#f]
 otherwise.


If the current eventspace is not the initial eventspace, this
procedure returns @scheme[void] (when called with zero arguments)
or has no effect (when called with a handler).
}

@defproc*[([(application-file-handler)
            (path? . -> . any)]
           [(application-file-handler [handler-proc (path? . -> . any)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a procedure that is called under Mac OS X
 and Windows when the application is running and user double-clicks an
 application-handled file or drags a file onto the application's
 icon. The procedure is always called in the initial eventspace's
 handler thread (as a callback), and the argument is a filename.

The default handler queues a callback to the
@method[window<%> on-drop-file] method of the most-recently activated frame in the main eventspace (see
@scheme[get-top-level-edit-target-window]), if
 drag-and-drop is enabled for that frame.

When the application is @italic{not} running and user double-clicks an
 application-handled file or drags a file onto the application's icon,
 the filename is provided as a command-line argument to the
 application.

If the current eventspace is not the initial eventspace, this
procedure returns @scheme[void] (when called with zero arguments)
or has no effect (when called with a handler).
}
