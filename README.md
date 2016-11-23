# About Gauche-makiki

## Overview

Gauche-makiki is a simple multithreaded http server intended for
applications that want to provide http server capability easily.
The main functionalities are available by just one file, `makiki.scm`,
so you can either install it as an ordinary Gauche extension library,
or you can just copy the file into your application.

You need Gauche 0.9.5 or later to use Gauche-makiki.


## Getting started

You'll get the idea by looking at the [minimal server](examples/minimal.scm):

    (use makiki)
    (define (main args) (start-http-server :port 6789))
    (define-http-handler "/"
      (^[req app] (respond/ok req "<h1>It worked!</h1>")))

Basically, you register a handler for a path (or a pattern for a path),
and the server dispatches matching request to the handler, which is
expected to return the content.

The `req` argument holds the information of the request, and
the `app` argument holds the application state you pass to
`start-http-server`.  (The above example isn't using application
state.  See [this BBS example](examples/query.scm) for simple
usage of application state.)

Gauche-makiki isn't an all-in-one framework; rather, we provide
simple and orthogonal parts that can be combined together
as needed.


## Handling requests

### Registering handlers

To use the server, you should define _http-handler_ using
`define-http-handler` macro:

    (define-http-handler [METHODS] PATTERN [? GUARD-PROC] HANDLER-PROC)

Or, handlers can be added procedurally using `add-http-handler!`:

    (add-http-handler! PATTERN HANDLER-PROC :optional GUARD-PROC METHODS)

`METHODS` is a list of symbols (`GET`, `POST`, etc.) that this handler
accepts.  You can define different handler with the same `PATTERN`
as far as `METHODS` don't overlap.   When omitted,
`(GET HEAD POST)` is assumed.

`PATTERN` can be a regexp or a string.

For each incoming request, the server matches its path of
the request uri against `PATTERN`.  If `PATTERN` is a string,
entire request path must match exactly to the pattern.  If `PATTERN`
is a regexp, `rxmatch` is used.  When the request path matches,
the server calls `HANDLER-PROC` with two arguments:

    (handler-proc REQUEST APP-DATA)

`REQUEST` is a request record, explained below.
`APP-DATA` is an application-specific data given at the time the server
is started.  Gauche-makiki treats `APP-DATA` as opaque data; it's
solely up to the application how to use it.

The optional `GUARD-PROC` is a procedure called right after the
server finds the request path matches `PATTERN`.
It is called with two arguments,
`REQUEST` and `APP-DATA`.  If the guard proc returns false,
the server won't call the corresponding handler and look for
another match instead.
It is useful to refine the condition the handler is called.

If the guard procedure returns a non-false value, it is stored in the
`guard-value` slot of the request record, and available to the
handler procedure.
See [examples/session.scm](examples/session.scm) for an example
of using a guard procedure and the request's `guard-value` slot.


### Request record

A request record passed to the handlers and guard procedures
has the following slots (only public slots are shown):

      line                ; request line
      socket              ; client socket  (#<socket>)
      remote-addr         ; remote address (sockaddr)
      method              ; request method (symbol in upper cases, e.g. GET)
      uri                 ; request uri
      http-version        ; requested version (e.g. "1.1")
      server-host         ; request host (string)
      server-port         ; request port (integer)
      path                ; request path (string, url decoded)
      path-rxmatch        ; #<regmatch> object of matched path
      guard-value         ; the result of guard procedure
      query               ; unparsed query string
      params              ; query parameters (result of cgi-parse-parameters)
      headers             ; request headers (result of rfc822-read-headers)
      (response-error)    ; #f if response successfully sent, #<error> otherwise.
                          ;  set by respond/* procedures.  The handler can check
                          ;  this slot and take actions in case of an error.

The following convenience procedures are avaiable on the request record.

    (request-iport REQ)     ; input port to read from the client
    (request-oport REQ)     ; output port to write to the client.
                            ;  NB: the handler proc shouldn't write
                            ;  to this port normally---one of the 
                            ;  'respond' procedures below takes care of
                            ;  writing response line and headers.

    (request-param-ref REQ PARAM-NAME . keys)
                            ; Retrieve request query-string parameter with
                            ; PARAM-NAME.  KEYS are a keyward-value list
                            ; passed to cgi-get-parameter in www.cgi.
                            ; See also `let-params` below for easier access.
    (request-header-ref REQ HEADER-NAME :optional (DEFAULT #f))
                            ; retrieve the value from the request headers.
                            ; See also `let-params` below for easier access.
    (request-cookies REQ)   ; returns parsed cookie list (see rfc.cookie)
                            ; in the request.
    (request-cookie-ref REQ COOKIE-NAME :optional (DEFAULT #f))
                            ; returns one entry of the parsed cookie with
                            ; the given COOKIE-NAME.  The returned value
                            ; is the result of `parse-cookie-string` of
                            ; `rfc.cookie`, i.e.
                            ; `(<name> <value> <cookie-parameters> ...)`
                            ; See also `let-params` below for easier access.

The handler procedure can set/modify response headers using
the following procedures.

    (response-header-push! REQ HEADER-NAME VALUE)
    (response-header-delete! REQ HEADER-NAME)
    (response-header-replace! REQ HEADER-NAME VALUE)
    (response-cookie-add! REQ NAME VALUE . COOKIE-OPTIONS)
    (response-cookie-delete! REQ NAME)

(NB: `response-cookie-delete!` merely removes the named cookie form
the response message; it does not remove the cookie from the client.)


### Accessing parameters passed by client

It is often the case that the server needs to generate the content
based on parameters clients provide.  Those parameters can come from
several different sources; most commonly via query parameters in url
or form-encoded in POST body, but can also be via request path
component (often the case in REST API) or sometimes via cookies
or even via request headers.   The `let-params` macro provides
an easy and convenient way to access those parameters.

    (let-params REQ (VAR-SPEC ...) BODY ...)

`REQ` should be the request record.  Each `VAR-SPEC` specify
a variable and its source, in one of the following forms:

    (var source kv-args ...)
    (var)
    var

Where `var` is a symbol (variable name), `source` is a string,
and `kv-args` is keyword-value list.  This form extracts parameters
according to `source` and binds its value to `var`, then executes
`body` ....  The latter two forms are a shorthand for `(var "q")`.

The `source` string can have either `<kind>:<name>` or just `<kind>`,
where `<kind>` is a single character specifying where the value
should be taken.

    q  - Query parameters
    p  - Path regexp matches
    c  - Cookies
    h  - Request headers

The optional `<name>` part specifies the parameter's name as sent
from the client, e.g. query name, cookie name or header name.
For path regexp match, `<name>` can be a word for named subgroup,
or an integer for unnamed subgroups.  If `<name>` is omitted, the
name of `var` is assumed.

The following keyword arguments are accepted in `kv-args`.

    :default <value>    Specifies the default value when the
                        parameter isn't provided from the client.
                        The default default value is `#f`, except
                        for the list query parameters, in which case
                        the default default value is `()`.

    :convert <proc>     Specifies the converter procedure `<proc>`,
                        which should take a string and convert
                        it to suitable type of object.  Note:
                        if the value isn't provided, `<proc>`
                        is never called and the default value is
                        directly used.

    :list <flag>        This is only effective for query parameters.
                        If `<flag>` is a true value, the client
                        can specify multiple instances of the same
                        name of query parameters, and all the values
                        are gathered to a list.  If `:convert` is
                        also given, the convert procedure is applied
                        to each value.  If `:default` is also given,
                        its value is only used when there's no
                        parameter for this name is provided.

Suppose you have in the following code:

    (define-http-handler #/^\/resource\/(\d+)\/edit$/
      (^[req app]
        (let-params req ([name        "q"]
                         [comment     "q:c"]
                         [resource-id "p:1" :convert x->integer]
                         [sess        "c:sessionid"])
          ...)))

And if the client sends this request:

     http://..../resource/33525/edit?name=foo&c=bar%20baz

Then the code gets `name` to be bound to `"foo"`, `comment` to
be bound to `"c"`, `resource-id` to be bound to 33525.  (`Sess`
would depend on whether the client send a cookie for `"sessionid"`.)

Of course, the client can send query parameters via POST, but in
that case, the server needs to use `with-post-parameters` (see below)
to fold the parameters in the request body into `request-params`
for `let-params` to process them.


### Response

`HANDER-PROC` should call one of the following respond procedure at
the tail position.   NB: These must be extended greatly to support
various types of replies.

    (respond/ok REQ BODY :key CONTENT-TYPE)
                            ; This returns 200 response to the client,
                            ; with BODY as the response body.  See below
                            ; for allowed values in BODY.
                            ; CONTENT-TYPE argument can override the default
                            ; content-type inferred from BODY.

    (respond/ng REQ CODE :key BODY CONTENT-TYPE)
                            ; This returns CODE response to the client.
                            ; If BODY keyword arg is omitted, the body
                            ; consists of the description of the HTTP code.
                            ; See below for allowed values in BODY.

    (respond/redirect REQ URI :optional (CODE 302))
                            ; Send back a redirection message using Location
                            ; header.  URI can be an absolute uri or
                            ; just a path component; in the latter case,
                            ; protocol, host and port compoents are 
                            ; automatically added.

These procedures return after the entire message is sent.  If an error
occurs during sending the message (most likely because the client
has disconnected prematurely), an error condition is stored in
(request-response-error REQ).

The response body for `respond/ok` and `respond/ng` can be one of
the following forms.  The content type can be overridden by
`CONTENT-TYPE` keyword argument.

* _string_ : A string is sent back as `text/plain; charset=utf-8`.

* _text-tree_ : A tree of strings; see `text.tree`.  Concatenated string
is sent back as `text/plain; charset=utf-8`.

* _u8vector_ : The content of the vector is sent back as
`application/binary`.

* (`file` _filename_) : The content of the named file is sent back.
Content-type is determined by the file's extension by default.
See the description of `file-handler` below for the details of
content-type handling.

* (`plain` _lisp-object_) : The lisp object is converted to a string
by `write-to-string`, then sent back as `text/plain; charset=utf-8`.

* (`json` _alist-or-vector_) : The argument is converted to a JSON
by `construct-json-string` (see `rfc.json`), then sent back as
`application/json; charset=utf-8`.

* (`sxml` _sxml_) : The SXML tree is rendered by to XML or HTML. (If
the root node of _sxml_ is `html`, then `sxml:sxml->html` is used to
render to HTML with content type `text/html; charset=utf-8`.  Otherwise
`sxml:sxml->xml` is used to render to XML, with conten type
`application/xml`.

* (`chunks` _string-or-u8vector_ ...) : Chunks are concatenated
and sent back as `application/octed-stream`.  This form allows
you to pass a lazy list, so that you can avoid creating entire
content in memory.

Check out scripts in `examples` directory for some concrete examples.


### Errors and response

You can raise a condition `<request-error>` to nofity the client that
you encounter an error during processign the request.  Use the
`request-error` procedure to raise the condition:

    (request-error :key status body content-type)

The `status` keyword argument is used as the http response status,
defaulted by 400.  The `body` and `content-type` arguments are
the same as in `respond/ok` and `respond/ng`.  This is useful to
abort the processing of a request deep in the stack.

For example, you can return an error message in JSON form to the
client as follows:

    (unless (parameter-is-valid?)
      (request-error :body `(json (("message" . "Invalid parameter")))))

If you raise an unhandled condition other than `<request-error>` from
the handler, it is captured by makiki and the client receives
`500 Internal Server Error` response, and the error itself is logged
to the error log.


## Built-in handlers

For typical tasks, we provide convenience procedures to build a
suitable handler.  The following procedures return a procedure
that can be directly passed to `define-http-handler`; for example,
the following handler definition serves files under `document-root`:

    (define-http-handler "/"  (file-handler))

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

    (document-root)

A parameter that holds the current path of the document root (the one
given to `start-http-server`; `"."` by default.)

The `Last-modified` response header is generated by this handler
automatically, based on the the timestamp of the file.


### Calling CGI scripts

There's an experimental support to call a CGI script written
in Gauche.  Instead of spawning a child process, we load
Gauche program and call its main routine "in process".
You have to `(use makiki.cgi)` to use this feature.

    (cgi-script FILE :key ENTRY-POINT SCRIPT-NAME LOAD-EVERY-TIME)

Loads the cgi script in FILE, and creates and returns a cgi handler that
calls a procedure named by ENTRY-POINT inside the script (`main` by default).

To avoid interference with makiki itself, the script is loaded
into an anonymous module.  

Loading is done only once unless LOAD-EVERY-TIME is true.
Usually, loading only once cuts the overhead of script loading for
repeating requests.  However, if the cgi script sets some global
state, it should be loaded for every request---a script can
be executed concurrently by multiple threads, so any code
relying on a shared mutable global state will fail.
Note also that we assume the script itself isn't written inside
a specific module; if it has it's own define-module and
select-module, the module will be shared for every load, and
we won't have enough isolation.

The cgi script should access to cgi metavariables through
`cgi-get-metavariable` (in `www.cgi` module), not directly
from the environment variables.

SCRIPT-NAME is the path to the script *in the URL*.  That is,
if the script is accessible via `http://example.com/foo/bar/baz.cgi`,
then it should be `"/foo/bar/baz.cgi"`.  It doesn't need to be
related to the actual pathname of the cgi script file.  The value
becomes the value of `SCRIPT_NAME` CGI metavariable, and also
used to calculate `PATH_INFO` CGI metavariable.

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


## Starting and terminating the server

Finally, to start the server, call `start-http-server`.

    (start-http-server :key host port path document-root num-threads max-backlog
                            access-log error-log forwarded? app-data
                            startup-callback shutdown-callback
                            control-channel)


    host (#f or string), port (integer) - Passed to make-server-sockets
       of gauche.net to open the server socket.  The default values are
       #f and 8080.

    path (#f or string) - If a string is given, it specifies the path to
       a Unix-domain socket on which the server listens.  In that case
       `host` and `port` arguments are ignored.

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

    forwarded? - specify true if you use makiki behind a reverse-proxy httpd,
       and access-log uses the value of x-forwarded-for header if exists,
       instead of the client's address.

    app-data - an opaque data passed to the request handler as is.

    startup-callback - a procedure to be called after the server opened
       sockets, but before start processing any requests.  A list of
       server sockets are passed as the only argument.  Logging procedures
       are already active.

    shutdown-callback - a thunk to be called after all the server operations
       are shut down.  If given, this is the last thing `start-http-server`
       does before returning.

    control-channel - an opaque object, through which you can request
       the server loop to shutdown.  See `make-server-control-channel` and
       `terminate-server-loop` below.

Note that `start-http-server` enters the server loop and won't return
by default.  There are two ways to shut down the server loop.

* Send `SIGINT` or `SIGTERM`.  By default, signals sent to a Gauche process
  is handled by the main thread.  So if you called `start-http-server`
  in the main thread, this is the easiest way.
* If you run `start-http-server` outside of the main thread, or don't want
  to rely on signals, you need a bit of setup.  (1) Create a server control
  channel by `make-server-control-channel` and pass it to `:control-channel`
  argument of the `start-http-server`.  (2) When you want to request
  the server loop to shutdown, call `terminate-server-loop` with the
  control channel.  See below for further description.  See
  [this test code](tests/termination.scm) as an example.


Here's the API for controlling the server loop.

    (make-server-control-channel)

Returns an opaque object through which you can request the server loop to
shut down.   You need one channel for each server loop, if you have more
than one loop.

    (terminate-server-loop CHANNEL EXIT-CODE)

Calling this function causes `start-http-server` that has CHANNEL
to break the loop, does cleaning up (including finishing request that are
already being processed), and returns EXIT-CODE.

You can pass any object to EXIT-CODE, but the supposed way to call
`start-http-server` is the tail position of the `main` function; in
that case, EXIT-CODE becomes the exit code of the server.

    (define (main args)
       ...
       (start-http-server ...))


## Add-ons

Some less frequently used features are provided in separate modules.

* `makiki.connect`: Handling `CONNECT` http request.
See [simple proxy example](examples/proxy.scm).

* `makiki.cgi` : Handling CGI scripts.


## Examples

The [examples](examples/) directory contains some simple server scripts,
each one shows how to implement a specific fuctionality.

