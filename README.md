Varr
====

Simple http-server with redis support

Building
--------

`rebar get-deps compile generate`

This builds the application and it's dependencies.

Configuration
-------------

File `rel/files/sys.config` is responsible for managing the application config.

Right now application supports following config variables:

 * `http_port` - specifies a port on which the server will listen
 * `parser_pool_size` - specifies size of parser workers pool

Usage
-----

    bash rel/var/bin/var <command>

    command = start | stop | console

`start` starts the daemon (default port is 8080), `stop` stops it, `console` starts a process with Erlang REPL attached to it.

Server listens for POST messages containing JSON, generates key for them and places body of the request, as well as some additional info, into the redis database.
