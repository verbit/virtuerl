virtuerl
=====

An OTP application

Build
-----

    $ rebar3 compile


## Running

On the server

```sh
sudo -s ./erts-13.1.5/bin/erl -mode embedded -boot releases/0.7.0+build.61.ref8fc0b7e/start -config releases/0.7.0+build.61.ref8fc0b7e/sys.config -proto_dist inet6_tcp -name verbit@verbit.in-berlin.de -setcookie abcdef
```

Locally
```sh
rebar3 compile
erl -name moi -proto_dist inet6_tcp -setcookie abcdef -pa _build/default/lib/*/ebin -hidden
```
