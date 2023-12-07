# Developing a server with `makiki.dev`

A typical server written in Makiki calls `start-http-server` in the
`main` procedure to process requests.  It won't return until the server
is terminated, so if you call it in REPL, you can't interact with
the process any further.

The `makiki.dev` module improve the situation.  It runs the server
in a separate thread so that you can keep working in REPL, and also
it supports reloading of the script so that you can incrementally
improve the program without stopping the server.

## Walk-through

## API

### `(start-server! [SCRIPT-PATH])`

Load the server script specified by `SCRIPT-PATH` and runs the
server in a separate thread.

It remembers `SCRIPT-PATH`, so you can omit it in the subsequent
calls.  It also watches updates of the script file, and whenever
the file's mtime changes, it automatically reloads it.

The script is loaded into a module `makiki.user`, to avoid interfering
with whatever you're doing in the default `user` module.

Running the server is a bit tricky; typically, Makiki server script
creates and initializes an application object and then calls
`start-http-server` in `main` procedure.
We don't know how you construct the application object,
so we merely call `main` within a separate thread.  The thread exits
when `main` returns, which typically means `start-http-server` exits.

If you need a different setup for development rather than the actual
deployment, you can define `dev-main` procedure in the script as well.
If `dev-main` exists, `start-server!` calls it instead of `main`.

If the server is already running, it does not restart the server
thread.  However, before loading the script, it empties http handler
database, so reloading the script replaces entire handlers.

Since reloading doesn't call `main` or `dev-main` again, changes
in the application object may not be reflected by mere reloading.
In such a case, you need to call `stop-server!` and then `start-server!`
to reinitialize application and restart the server loop.

### `(stop-server!)`

Stops the currently running server loop.  If a server loop isn't running,
this is no-op.

Stopping the server is done via implicit server control channel
(see `make-server-control-channel` and `terminate-server-loop` in
[README.md](../README.md).)  So if there're pending requests, you
server loop processes them before exitting.
