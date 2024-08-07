# About Gauche-makiki

## Table of contents

  * [Overview](#overview)
  * [Getting started](#getting-started)
  * [Handling requests](#handling-requests)
     * [Registering handlers](#registering-handlers)
     * [Routing](#routing)
     * [Request record](#request-record)
     * [Accessing parameters passed by client](#accessing-parameters-passed-by-client)
     * [Handling POST/PUT request body](#handling-postput-request-body)
        * [Form encoded data](#form-encoded-data)
        * [Json](#json)
        * [Retrieving raw body](#retrieving-raw-body)
        * [Roll your own reader](#roll-your-own-reader)
     * [Response](#response)
     * [Errors and response](#errors-and-response)
  * [Built-in handlers](#built-in-handlers)
     * [Serving files](#serving-files)
     * [Calling CGI scripts](#calling-cgi-scripts)
     * [Modifying headers](#modifying-headers)
  * [Logging and tuning](#logging-and-tuning)
     * [Logging](#logging)
     * [Profiling](#profiling)
  * [Starting and terminating the server](#starting-and-terminating-the-server)
  * [Add-ons](#add-ons)
  * [Examples](#examples)

## Overview

Gauche-makiki is a simple multi-threaded http server intended for
applications that want to provide http server capability easily.
The main functionalities are available by just one file, `makiki.scm`,
so you can either install it as an ordinary Gauche extension library,
or you can just copy the file into your application.

You need Gauche 0.9.7 or later to use Gauche-makiki.
To serve over secure connection (https), you need Gauche 0.9.14 or later
with TLS support.


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

See also [development.md](doc/development.md) to develop server
script interactively with REPL.

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

`PATTERN` is to specify routing from the request path to the handler.
It is explained in the Routing section below.

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


### Routing

`PATTERN` argument of `define-http-handler` can be a string,
a list, or a regexp,

- If it is a string, it matches with the exactly same request path.  It
  is suitable for resources with a fixed path.
- If it is a list, each element must be either a string or a symbol.
  The request path is split with `/` into a list of path components,
  and each component is compared to the corresponding element of `PATTERN`.
  If the pattern's element is a string, it must exactly match the path
  component.  If the element is a symbol, it matches unconditionally to
  the corresponding component, and the match is saved to `request-path-match`.
  If the list is a dotted list, the last `cdr` must be a symbol, and it
  matches any remaining components.
- If it is a regexp, it is matched against the entire request path.
  If matched, the match object is saved to `request-path-match` so that
  submatches can be retrieved later.

For each incoming request, the server matches its path of
the request uri against `PATTERN`.  Note: For component-wise match
with a list pattern, trailing slashes of request path is ignored; that is,
`("foo" x)` matches `"/foo/bar"` and `"/foo/bar/"`.

When the request path matches, the server calls `HANDLER-PROC` with
two arguments:

    (handler-proc REQUEST APP-DATA)

Some routing examples:

    ;; Fixed path.  The handler is called when the client requests
    ;; /favicon.ico.
    (define-http-handler "/favicon.ico" handler)

    ;; Path with parameter.  Paths such as "/usr/bob" or "/usr/alice"
    ;; match, and the variable part can be retrieved with
    ;; ((request-path-match req) 'user-id).
    (define-http-handler ("user" user-id) handler)

    ;; Path with parameter and conversion.  Obj-id part only matches
    ;; a string valid as an integer, e.g. "/obj/233515".  The matched
    ;; part is converted to an integer with the procedure path:int.
    (define-http-handler ("obj" (path:int obj-id)) handler)

    ;; If the pattern is a dotted-list, all the rest path components
    ;; are saved with the name of the last cdr.  The following route
    ;; matches "/src/a/b/c.txt", for example, and
    ;; ((request-path-match req) 'path) gives "a/b/c.txt".
    (define-http-handler ("src" . path) handler)

There are a few utility procedures to help parsing a path component.
They take a path component as a string, and returns #f if it is not
accepted, or an object if accepted.

    (path:int COMPONENT)

If the path component string COMPONENT is a valid exact decimal integer
representation, returns that exact integer; otherwise returns #f.

    (path:hex COMPONENT)

If the path component string COMPONENT is a valid exact hexadecimal
integer representation, returns that exact integer; otherwise returns #f.

    (path:uuid COMPONENT)

If the path component string COMPONENT is a valid representation of
UUOD, returns an UUID object; otherwise returns #f.  See
[the document of rfc.uuid module](https://practical-scheme.net/gauche/man?p=rfc.uuid)
for the details of UUID representation.

The value of the matched component can be retrieved with passing
the key value to `(request-path-match req)`, or, more conveniently,
using `request-path-ref`.  See below.


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
      secure              ; whether the communication is secure
                          ;  This can be
                          ;    - #t (we have direct TLS connection),
                          ;    - 'forwarded (we have reverse proxy that has
                          ;       secure connection to the client),
                          ;    - #f (otherwise)
      path                ; request path (string, url decoded)
      path-match          ; proc to extract matched path component
      guard-value         ; the result of guard procedure
      query               ; unparsed query string
      params              ; query parameters (result of cgi-parse-parameters)
      headers             ; request headers (result of rfc822-read-headers)
      (response-error)    ; #f if response successfully sent, #<error> otherwise.
                          ;  set by respond/* procedures.  The handler can check
                          ;  this slot and take actions in case of an error.

The following convenience procedures are available on the request record.

    (request-iport REQ)     ; input port to read from the client
    (request-oport REQ)     ; output port to write to the client.
                            ;  NB: the handler proc shouldn't write
                            ;  to this port normally---one of the
                            ;  'respond' procedures below takes care of
                            ;  writing response line and headers.

    (request-params REQ)    ; List of query-string parameters, in the form
                            ; of ((NAME VALUE ...) ...) where NAMEs and VALUEs
                            ; are all strings.  It is compatible to the
                            ; reutrn value of cgi-parse-parameters.
    (request-param-ref REQ PARAM-NAME . keys)
                            ; Retrieve request query-string parameter with
                            ; PARAM-NAME.  KEYS are a keyword-value list
                            ; passed to cgi-get-parameter in www.cgi.
                            ; See also `let-params` below for easier access.
    (request-headers REQ)   ; List of request headers, in the form of
                            ; ((NAME VALUE) ...).  It is compatible to the
                            ; header list used in rfc.822 module.
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
    (request-path-ref REQ KEY :optional (DEFAULT #f))
                            ; returns the value matched to the path component
                            ; designated with KEY.  If the input path did not
                            ; match they component with KEY, DEFAULT is
                            ; returned instead.

The handler procedure can set/modify response headers using
the following procedures.

    (response-header-push! REQ HEADER-NAME VALUE)
    (response-header-delete! REQ HEADER-NAME)
    (response-header-replace! REQ HEADER-NAME VALUE)
    (response-cookie-add! REQ NAME VALUE . COOKIE-OPTIONS)
    (response-cookie-delete! REQ NAME)

If a header with the same name is pushed more than once, they all
appear in the response.  If you want to override the previously
pushed header, use `response-header-replace!`.

If a cookie with the same name is added, it replaces the previous
one if any.  Note that `response-cookie-delete!` does send back
a cookie to the client, but with the past expiration time so that
the client will delete it on its end.

By default, `secure` attribute of the cookie is automatically added
if `request-secure` has a true value.  You can turn off this behavior
by setting the parameter `add-secure-cookie` to false.


### Accessing parameters passed by client

After a handler is selected according to the request url and method,
query parameters in the url is parsed and saved in `request-params`.
(Note: Parameters passed via `POST` request body are not
parsed automatically; see *Handling POST request body* below.)

It is often the case that the server needs to look at several
different places to see what parameters the client provides;
most commonly they are passed via query parameters in url
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
be bound to `"bar"`, `resource-id` to be bound to 33525.  (`Sess`
would depend on whether the client send a cookie for `"sessionid"`.)

### Handling POST/PUT request body

A query string in a request url is automatically parsed and
accessible via `request-query`, `request-params` and `request-param-ref`,
but the parameters passed via POST/PUT body aren't processed by default.

#### Form encoded data

The following procedure returns a handler that parses POST request body
and put the parsed result to `request-params`:

    (with-post-parameters INNER-HANDLER :key PART-HANDLERS)

It can handle the body with both `multipart/form-data` and
`application/x-www-form-urlencoded` content types.

The REQUEST structure the INNER-HANDLER receives got parsed parameters
(If the original request also has a query string in url, that will be
overwritten.)

PART-HANDLERS specifies how to handle each parameter is handled
according to its name.  By default, all parameter values are
read into strings.  However, you might not want that behavior if
you're accepting large file updates.  See the documentation of
[`www.cgi`](http://practical-scheme.net/gauche/man/?p=www.cgi) module
for the meaning of PART-HANDLERS.

#### Json

For Web APIs, it is convenient to receive request in json.  Here's
a convenience wrapper:

    (with-post-json INNER-HANDLER :key ON-ERROR)

It parses the request body as json, and set the parsed value in
`request-params` with the key "json-body".  That is, INNER-HANDLER
can access the parsed json by `(request-param-ref req "json-body")`.
If the client didn't pass the body, the "json-body" parameter is `#f`.

Json dictionary becomes an alist, and json array becomes a vector.
See the documentation of
[`rfc.json`](http://practical-scheme.net/gauche/man/?p=rfc.json), for
the details.

ON-ERROR is a procedure that takes three argument, as
`(on-error req app condition)`, and called when a `<json-parse-error>`
is raised during parsing.  It must either return an alternative value
to be used as the value of "json-body", or call `request-error` to
notify the client the error.  If omitted or `#f`, the procedure
returns 400 error to the client.


#### Retrieving raw body

If you don't use one of the POST handlers above, the request body
hasn't been read when the handler is called.  The easiest way to
retrieve the request body at once is using `read-request-body`:

    (read-request-body req)

This reads the request body into a fresh u8vector and returns it.
It can return `#f` if the request has no body.

If the body has already read (even partially), or ended prematurely
(i.e. the data is smaller than the size stated by content-length),
this procedure returns `#<eof>`.


#### Roll your own reader

If you need to handle request body specially (for example, if the client
sends huge binary data, you don't want to read everything into memory),
you can handle it by yourself.  When the handler is called, the request
body is available to be read from `(request-iport req)`.  Check
the content-type header first, for it must specify the size of the request
body in octets.


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
                            ; protocol, host and port components are
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
`sxml:sxml->xml` is used to render to XML, with content type
`application/xml`.

* (`chunks` _string-or-u8vector_ ...) : Chunks are concatenated
and sent back as `application/octet-stream`.  This form allows
you to pass a lazy list, so that you can avoid creating entire
content in memory.

Check out scripts in `examples` directory for some concrete examples.


### Errors and response

You can raise a condition `<request-error>` to notify the client that
you encounter an error during processing the request.  Use the
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

The content-type and the body of such 500 response is determined by
the "Accept" request header; if the request has "Accept: application/json",
the error response is in JSON.  For the time being, we recognize
application/json and text/html, and for all other content-type we return
a plain text (text/plain).

By default, we only return "Internal Server Error" to the client,
which is a bit inconvenient during development.  If the environment
variable `MAKIKI_DEBUGGING` is set when the server is run, we add
the error message in the error response as well.  You can also turn on
the debugging feature by setting the parameter `debugging` to a true
value.   Make sure you don't
set the environment variable in the production environment, so that you
wouldn't reveal any internal information accidentally.


### Tweaking response headers and cookies

Response headers and cookies are sent back to a client inside one of
response procedures such as `respond/ok`.  When you're writing a
wrapper handler, there can be a case that you want to add response
headers or cookies after the internal handler generates content, but
before it is sent back to the client.  For example, you might send
a cookie depending on the parameter value which may be modified by
the internal handler.

You can register callbacks for that purpose.

   (respond-callback-add! (^[req code content-type] ...))

The callback list is empty when the handler is called.  If callbacks are
added during handling, they are called in the order of addition, with
three arguments: The request record, integer response code, and string
content-type.  At the time the callback is called, the content of the
response is already generated, but no information is sent back to the
client yet.  You can modify response headers and cookies there.  You
can't change the response content, though.

The callback must return `#f`.  We may allow other return values in future.


## Built-in handlers

For typical tasks, we provide convenience procedures to build a
suitable handler.  The following procedures return a procedure
that can be directly passed to `define-http-handler`; for example,
the following handler definition serves files under `document-root`:

    (define-http-handler "/"  (file-handler))

Some handler-builders takes another handler procedure and returns
a new handler that augments the original handler.

See [examples](examples/) for more usages.


### Serving files

For the convenience, file-handler can be used to create a handler
procedure suitable for define-http-handler to return a file
on the server.

    (file-handler :key (directory-index '("index.html" #t))
                       (path-trans request-path)
                       (root #f))

`PATH-TRANS` should be a procedure that takes `REQUEST` and returns
the server-side file path.  The returned path should start from
slash, and the document-root directory passed to the start-http-server
is prepended to it.  It is not allowed to go above the document
root directory by `"/../../.."` etc---403 error message would results.

The file to be served is determined as this: First, the path part
is extracted from the request with `PATH-TRANS`, then the directory
name given to `ROOT` argument is appended.  If `ROOT` is `#f`, the
value of the parameter `document-root` is used.

The `DIRECTORY-INDEX` keyword argument specifies the behavior when
given path is a directory.  It must be a list of either filename or
`#t`.  The list is examined from left to right; if it is a filename,
and the named file exists in the directory, the content of the file
is returned.  If it is a filename but does not exist, next element
is examined.  If it is `#t`, the list of the entries in the directory
is returned.

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

    (cgi-script FILE :key ENTRY-POINT SCRIPT-NAME LOAD-EVERY-TIME FORWARDED)

Loads the cgi script in FILE, and creates and returns an http handler that
calls a procedure named by ENTRY-POINT inside the script (`main` by default).

The return value can be used as the handler argument for `define-http-handler`,
as follows:

    (define-http-handler #/\/foo.cgi(\/.*)?/
      (cgi-script (build-path (sys-dirname (current-load-path))
                              "foo.cgi")
                  :script-name "/foo.cgi" :forwarded #t))

To avoid interference with makiki itself, the script is loaded
into an independent, anonymous module.

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

If you're running Makiki server behind a reverse proxy, give a true
value to FORWARDED.  Then Makiki adjust SERVER_NAME, SERVER_PORT, and
HTTPS cgi metavariables according to the external connection (between
the client and the proxy server), instead of the internal connection
(between the proxy and Makiki).  It is important to produce output
using what the client directly sees; e.g. a link to the same site
must use the external hostname and port, otherwise the client
can't follow the link.


    (cgi-handler PROC :key SCRIPT-NAME FORWARDED)

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


## Logging and tuning

### Logging

If you write out logs inside an http handler, you can use those
macros:

    (access-log FMT ARGS ...)
    (error-log FMT ARGS ...)

FMT and ARGS are the same as `log-format` in `gauche.logger`.
The destination of logs are set by the keyword arguments
of `start-http-server` described below.

### Profiling

You can run Gauche's built-in sampling profiler during handling
a request.  There are two ways to do it.

If the environment variable `MAKIKI_PROFILER_OUTPUT` is set
when `makiki.scm` is loaded, Makiki automatically profiles all
handlers.  The value of `MAKIKI_PROFILER_OUTPUT` is used as a
filename to which the profiling result is written out.  If the
file already exists, the result is appended to it.  Each result is preceded
by the request path.

Alternatively, you can selectively profile specific handlers
by wrapping the handler with `with-profiling-handler`:

    (with-profiling-handler OUTPUT-FILE INNER-HANDLER)

This returns a procedure suitable to be a handler.  When called,
it runs `INNER-HANDLER` with profiler running, and then write out
the result to `OUTPUT-FILE`.   When `OUTPUT-FILE` already exists,
the result is appended to it.

You should use either one of the above method, but not both;
`MAKIKI_PROFILER_OUTPUT` tries to profile every handler, even it
is already wrapped by `with-post-parameters`.

Note: If multiple handlers run simultaneously in multiple threads,
the profiling result becomes less reliable, for you don't know
which thread the sampling picks---the profiling is recorded per thread,
but the sampling timer is shared.  Make sure to issue one request
at a time during profiling.

See the [Using profiler](http://practical-scheme.net/gauche/man/?p=Using+profiler) section of the Gauche reference manual
for the details of Gauche's built-in profiler.


## Starting and terminating the server

Finally, to start the server, call `start-http-server`.

    (start-http-server :key app-data host port tls-port path
                            document-root num-threads max-backlog
                            access-log error-log
                            tls-settings
                            forwarded? auto-secure-cookie
                            startup-callback shutdown-callback
                            control-channel)


It takes the following keyword arguments:

- `app-data` - an opaque data passed to the request handler as is.
- `host` (`#f` or string), `port` (`#f` or integer) - Passed to
`make-server-sockets` of `gauche.net` to open the server socket.
If none of `port`, `tls-port` and `path` arguments are given, port 8080
is used for the convenience.
- `tls-port` (`#f` or integer) - Opens and listens TLS connection on
this port.  See [Running https server](doc/https.md) for how
to run https server.
- `path` (`#f` or string) - If a string is given, it specifies the path to
a Unix-domain socket on which the server listens.
- `document-root` - used to specify the root of the document served
by `file-handler`.  The default is the process's working directory.
- `num-threads` - number of threads to serve the request.  Currently threads
are created when the server is started.  The default is 5.
- `max-backlog` - max number of request queued when all threads are busy.
When a request comes while the queue is full, 503 (server busy)
response is returned to the client.   The default is 10.
- `access-log`, `error-log` - specify the destination of logs.  `#f` (no log),
`#t` (stdout), string (filename) or `<log-drain>` object.
For access log, `<log-drain>` is better not to have prefix, for
timestamp is included in the message.  The default is `#f`.
- `tls-settings` (keyword-value list): Pathnames for certificates and
private keys.  See [Running https server](doc/https.md) for details.
- `forwarded?` - specify true if you use makiki behind a reverse-proxy httpd,
and access-log uses the value of `x-forwarded-for` header if exists,
instead of the client's address.
- `auto-secure-cookie` - if this is true, 'secure' attribute of cookies
are automatically turned on when the request is over a secure
channel.  The default is `#t`.
- `startup-callback` - a procedure to be called after the server opened
sockets, but before start processing any requests.  A list of
server sockets are passed as the only argument.  Logging procedures
are already active.
- `shutdown-callback` - a thunk to be called after all the server operations
are shut down.  If given, this is the last thing `start-http-server`
does before returning.
- `control-channel` - an opaque object, through which you can request
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
See the section [Calling CGI scripts](#calling-cgi-scripts) above.

* `makiki.dev` : Interactive REPL development while running a server.
See [development.md](doc/development.md) for the details.

## Examples

The [examples](examples/) directory contains some simple server scripts,
each one shows how to implement a specific functionality.
