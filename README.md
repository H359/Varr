Var
===

Simple http-server with redis support

Building
--------

`rebar get-deps compile generate`

This builds the application and it's dependencies.

Usage
-----

`bash rel/var/bin/var <command>`

`command = start | stop | console`

`start` starts the daemon (default port is 8080, config files are not yet supported), `stop` stops it, `console` starts a process with Erlang REPL attached to it.