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

## Testing with client
- Go into the <code>apps/client</code> folder
- From a terminal, run the command:
```
$ rebar3 shell
```
For quick testing, you can run one of these commands:
- Test clients
```
$ client_app:test_clients().
```
That will create 5 different clients and connect them to the server
- Test rooms
```
$ client_app:test_rooms().
```
This will connect some clients and create different rooms, testing various cases of error and success.
- Test private messages
```
$ client_app:test_private_messages().
```
This will connect some clients and send some private messages.
- Test private rooms
```
$ client_app:test_private_rooms().
```
This will create one private and one public room, testing various cases.