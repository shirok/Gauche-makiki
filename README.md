# About Gauche-makiki

## Overview

Gauche-makiki is a simple multithreded http server intended for
applications that want to provide http server capability easily.
At this moment, the only required file is `makiki.scm`, so you can
either install it as an ordinary Gauche extension library, or you
can just copy the file into your application.

You need Gauche 0.9.3 or later to use Gauche-makiki.

## Handling requests

To use the server, you should define _http-handler_ using
`define-http-handler` macro:

    (define-http-handler REGEXP PROC)

Or, handlers can be added procedurally using `add-http-handler`:

    (add-http-handler! REGEXP PROC)

For each incoming request, the server matches its path of
the request uri against `REGEXP`, and if it matches, the server
calls `PROC` with two arguments.

    (proc REQUEST APP-DATA)

`REQUEST` is a request record (only publicly exposed slots are shown):

    (define-record-type request  %make-request #t
      line                ; the first line of the request
      socket              ; client socket  (#<socket>)
      remote-addr         ; remote address (sockaddr)
      method              ; request method (symbol in upper cases, e.g. GET)
      server-host         ; request host (string)
      server-port         ; request port (integer)
      path                ; request path (string, url decoded)
      path-rxmatch        ; #<regmatch> object of matched path
      query               ; unparsed query string
      params              ; query parameters (result of cgi-parse-parameters)
      headers             ; request headers (result of rfc822-read-headers)
      (response-error)    ; #f if response successfully sent, #<error> otherwise.
                          ;  set by respond/* procedures.  The handler can check
                          ;  this slot and take actions in case of an error.
      ...)

`APP-DATA` is an application-specific data given at the time the server
is started.

The following convenience procedures are avaiable on the request record.

    (request-iport REQ)     ; input port to read from the client
    (request-oport REQ)     ; output port to write to the client.
                            ;  NB: the handler proc shouldn't write
                            ;  to this port normally---one of the 
                            ;  'respond' procedures below takes care of
                            ;  writing response line and headers.

    (request-param-ref REQ PARAM-NAME . keys)
                            ; Retrive request query-string parameter with
                            ; PARAM-NAME.  KEYS are a keyward-value list
                            ; passed to cgi-get-parameter in www.cgi.
    (request-header-ref REQ HEADER-NAME :optional (DEFAULT #f))
                            ; retrieve the value from the request headers.
    (request-cookies REQ)   ; returns parsed cookie list (see rfc.cookie)
                            ; in the request.
    (request-cookie-ref REQ COOKIE-NAME :optional (DEFAULT #f))
                            ; returns one entry of the parsed cookie with
                            ; the given COOKIE-NAME.

The handler procedure can set/modify response headers using
the following procedures.

    (response-header-push! REQ HEADER-NAME VALUE)
    (response-header-delete! REQ HEADER-NAME)
    (response-header-replace! REQ HEADER-NAME VALUE)
    (response-cookie-add! REQ NAME VALUE . COOKIE-OPTIONS)
    (response-cookie-delete! REQ NAME)

(NB: `response-cookie-delete!` merely removes the named cookie form
the response message; it does not remove the cookie from the client.)


`PROC` should call one of the following respond procedure at the tail
position.   NB: These must be extended greatly to support various
types of replies.

    (respond/ok REQ BODY)   ; BODY can be <string>, (file <filename>),
                            ;   (plain <lisp-object>), (json <alist>),
                            ;   or text-tree (cf. text.tree)
                            ; This returns 200 response to the client,
                            ; with the specified content.

    (respond/ng REQ CODE)   ; This returns CODE response to the client.
                            ; the body consists of the description of the
                            ; HTTP code.

    (respond/redirect REQ URI :optional (CODE 302))
                            ; Send back a redirection message using Location
                            ; header.  URI can be an absolute uri or
                            ; just a path component; in the latter case,
                            ; protocol, host and port compoents are 
                            ; automatically added.

These procedures return after entire message is sent.  If an error
occurs during sending the message (most likely because the client
has disconnected prematurely), an error condition is stored in
(request-response-error REQ).

Check out scripts in `examples` directory for some concrete examples.


## Built-in handlers

For typical tasks, we provide convenience procedures to build a
suitable handler.  The following procedures return a procedure
that can be directly passed to `define-http-handler`; for example,
the following handler definition serves files under `document-root`:

    (define-http-handler #/^\/  (file-handler))

Some handler-builders takes another handler procedure and returns
a new handler that auguments the original handler.  

See [examples](examples/) for more usages.


### Serving files

For the convenience, file-handler can be used to create a handler
procedure suitable for define-http-handler to return a file
on the server.

    (file-handler :key (directory-index '("index.html" #t))
                       (path-trans request-path))

`PATH-TRANS` should be a procedure that takes `REQUEST` and returns
the server-side file path.  The returned path should start from
slash, and the document-root directory passed to the start-http-server
is prepended to it.  It is not allowed to go above the document
root directory by `"/../../.."` etc---403 error message would results.

Makiki uses some heuristics to determine `content-type` of the file,
but that's far from complete.   You can use a parameter `file-mime-type`
to customize the association of content-type and files; it must be a
procedure that takes one argument, the pathname of the file, and it
must return a mime-type in string, or `#f` to delegate the association
to the makiki's default handler.

    (docuement-root)

A parameter that holds the current path of the document root (the one
given to `start-http-server`; `"."` by default.)


### Calling CGI scripts

There's an experimental support to call a CGI script written
in Gauche.  Instead of spawning a child process, we load
Gauche program and call its main routine "in process".

    (cgi-script FILE :key ENTRY-POINT SCRIPT-NAME LOAD-EVERY-TIME)

Loads the cgi script in FILE, and creates and returns a cgi handler that
calls a procedure named by ENTRY-POINT inside the script (`main` by default).

To avoid interference with makiki itself, the script is loaded
into an anonymous module.  

Loading is done only once unless LOAD-EVERY-TIME is true.
Usually, loading only once cuts the overhead of script loading for
repeating requests.  However, if the cgi script sets some global
state, it should be loaded for every request---a script can
be executed concurrently, so any code relying on a shared mutable
global state will fail.
Note also that we assume the script itself isn't written inside
a specific module; if it has it's own define-module and
select-module, the module will be shared for every load, and
we won't have enough isolation.

The cgi script should access to cgi metavariables through
`cgi-get-metavariable` (in `www.cgi` module), not directly
from the environment variables.

SCRIPT-NAME is the path to the script *in the URL*.  That is,
if the script is accessible via `http://example.com/foo/bar/baz.cgi`,
then it should be `"/foo/bar/baz.cgi"`.  It isn't related to
the actual pathname of the cgi script file.  The value becomes
the value of `SCRIPT_NAME` CGI metavariable, and also used to
calculate `PATH_INFO` CGI metavariable.

    (cgi-handler PROC :key SCRIPT-NAME)

This is the low-level procedure that creates an http handler that
sets up the cgi metavariables and calls PROC, that takes one
argument (as in `main` procedure of the usual script; though
most cgi scripts won't use the argument).

PROC would write out the response to its stdout; which will
be captured by the created handler and returned to the client.



### Modifying headers

    (with-header-handler inner-handler header value ...)

This returns a handler that first adds extra response headers
then calls INNER-HANDLER.

Header is a keyword representing the header name; value can
be a string for header value, a procedure to take request and
app-data and to return a string header value, or #f to omit the
header.  For example, the following call returns a handler
that adds "Cache-control: public" header to the file response.

    (with-header-handler (file-handler) :cache-control "public")

Since that the headers are added before the inner handler is called,
they may be overwritten by inner-handler.


### Handling POST request parameters

A query string in a request url is automatically parsed and
accessible via `request-query`, `request-params` and `request-param-ref`,
but the parameters passed via POST body aren't processed by default.

The following procedure returns a handler that parses POST request body
and put the parsed result to `request-params`:

   (with-post-parameters INNER-HANDLER :key PART-HANDLERS)

The REQUEST structure the INNER-HANDLER receives got parsed parameters
(If the original request also has a query string in url, that will be
overwritten.)

PART-HANDLERS specifies how to handle each parameter is handled
according to its name.  By default, all parameter values are
read into strings.  However, you might not want that behavior if
you're accepting large file updates.  See the documentation of
[`www.cgi`] (http://practical-scheme.net/gauche/man/?p=www.cgi) module
for the meaning of PART-HANDLERS.


## Logging

If you write out logs inside an http handler, you can use those
macros:

    (access-log FMT ARGS ...)
    (error-log FMT ARGS ...)

FMT and ARGS are the same as `log-format` in `gauche.logger`.
The destination of logs are set by the keyword arguments
of `start-http-server` described below.


## Starting the server

Finally, to start the server, call `start-http-server`.

    (start-http-server :key host port document-root num-threads max-backlog
                            access-log error-log forwarded? app-data
                            startup-callback shutdown-callback)


    host (#f or string), port (integer) - Passed to make-server-sockets
       of gauche.net to open the server socket.  The default values are
       #f and 8080.

    document-root - used to specify the root of the document served
       by file-handler.  The default is the process's working directory.

    num-threads - number of threads to serve the request.  Currently threads
       are created when the server is started.  The default is 5.

    max-backlog - max number of request queued when all threads are busy.
       When a request comes while the queue is full, 503 (server busy)
       response is returned to the client.   The default is 10.

    access-log, error-log - specify the destination of logs.  #f (no log),
       #t (stdout), string (filename) or <log-drain> object.
       For access log, <log-drain> is better not to have prefix, for
       timestamp is included in the message.  The default is #f.

    forwarded? - specify #t if you use makiki behind a reverse-proxy httpd.
       Then access-log uses the value of x-forwarded-for header if exists,
       instead of the client's address.

    app-data - a opaque data passed to the request handler as is.

    startup-callback - a procedure to be called after the server opened
       sockets, but before start processing any requests.  A list of
       server sockets are passed as the only argument.  Logging procedures
       are already active.

    shutdown-callback - a thunk to be called after all the server operations
       are shut down.  If given, this is the last thing `start-http-server`
       does before returning.

## Examples

The [examples](examples/) directory contains some simple server scripts,
each one shows how to implement a specific fuctionality.

