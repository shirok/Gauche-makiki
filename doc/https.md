# Running https server

To serve over secure connections, you need Gauche release 0.9.14 or later.

You also need your server certificate, intermediate certificates, and
a private key.  Just to test in the local environment, you can generate
a self-signed certificate and a private key using `openssl`.  Check
the relevant document.

There are two keyword arguments of `start-http-server` that enables https
server:

- `tls-port`: Specifies the port number to accept TLS connections.
If you specify this but not the `port` argument, the server only listens to
TLS connection on this port.  If you specify the `port` argument as
well, the server listens to both ports, where `port` accepts plain
connections.
- `tls-settings`: This must be a keyword-value list and it can contain
the following elements:
  - `tls-certificates`: A list of pathnames to the server certificates,
  including intermediate certificates.
  - `tls-private-key`: A pathname to the private key file.
  - `tls-private-key-password`: (Optional) The password of the private key.
