Functionally Complete Backend
=============================

This is the backend for the new https://www.functionally-complete.com blog.

It consists of two major parts:

Server
------

The functionally-complete.cabal project is in the root directory. A `deploy.sh` script is provided, which will install the server as a service and start this service.

Reverse Proxy
-------------

The `reverse-proxy` directory contains a cabal project whose it is to provide a wrapper around the main server, to offer
- Redirect from HTTP to HTTPS
- Blue/Green deployment, to ensure that new versions of the server can be deployed without downtime.

Planned
-------

- A program that converts markdown into HTML suitable for the blog.
