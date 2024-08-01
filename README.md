Coding Challenge
=====

This is an Erlang based chat server.

Here are the instructions to run this application.

## Prerequisites
[Erlang/OTP25](https://www.erlang.org/downloads/25) and [rebar3](https://rebar3.org) must be installed on your machine to build and run this application.

## Run the server application

- Go into the <code>apps/server</code> folder
- From a terminal, run the command:
```
$ rebar3 shell
```
- Then, run the command:
```
$ application:start(server).
```

## Release and run project

- From a terminal, run the command:
```
$ rebar3 release
```
- Go inside the <code>_build/default/rel/coding_challenge/bin</code> folder
- From a terminal, run the command:
```
$ coding_challenge foreground
```
