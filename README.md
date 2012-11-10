# Overview

Gauche-makiki is a simple multithreded http server intended for
applications that want to provide http server capability easily.
At this moment, the only required file is `makiki.scm`, so you can
either install it as an ordinary Gauche extension library, or you
can just copy the file into your application.


# Handling requests

To use the server, you should define _http-handler_ using
`define-http-handler` macro:

    (define-http-handler REGEXP PROC)

For each incoming request, the server matches its path of
the request uri against `REGEXP`, and if it matches, the server
calls `PROC` with two arguments.

    (proc REQUEST APP-DATA)

`REQUEST` is a request record (only publicly exposed slots are shown):

    (define-record-type request  %make-request #t
      line                ; the first line of the request
      socket              ; client socket
      remote-addr         ; remote address (sockaddr)
      method              ; request method
      server-host         ; request host
      server-port         ; request port
      path                ; request path
      path-rxmatch        ; #<rxmatch> object of matched path
      query               ; unparsed query string
      params              ; query parameters
      headers             ; request headers
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


# Build-in handlers

## Serving files

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

## Modifying headers

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


# Starting the server

Finally, to start the server, call `start-http-server`.

    (start-http-server :key host port document-root num-threads max-backlog
                            access-log error-log forwarded? app-data)


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


     



