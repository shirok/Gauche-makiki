# Developing a server with `makiki.dev`

A typical server written in Makiki calls `start-http-server` in the
`main` procedure to process requests.  It won't unless until the server
is terminated, so if you call it in REPL, you can't interact with
the process any further.

The `makiki.dev` module improve the situation.  It runs the server
in a separate thread so that you can keep working in REPL, and also
it supports reloading of the script so that you can incrementally
improve the program without stopping the server.

## Walkthorugh


## API
