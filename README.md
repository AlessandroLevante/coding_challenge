Coding Challenge
=====

This is an Erlang based chat server.

Here are the instructions to run this application.

## Prerequisites
[Erlang/OTP25](https://www.erlang.org/downloads/25) and [rebar3](https://rebar3.org) must be installed on your machine to build and run this application.

## Third-Party Libraries
- [erlcloud](https://github.com/erlcloud/erlcloud): used to connect to DynamoDB and interact with it, using methods to add, update or remove data from tables.

# AWS access
It is possible to log in to AWS using an IAM user to see the created EC2 instances, DynamoDB status and other thing created through CloudFormation templates. Here are the credentials
for the AWS IAM user:
- [Console link](https://713881814113.signin.aws.amazon.com/console)
- Username: coding_challenge
- User ID: 713881814113

## Run the server application

- Open a terminal inside this project's base folder, then run the command:
```
$ rebar3 shell
```

## Release and run project

- Open a terminal inside this project's base folder, then run the command:
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

There are various other useful methods to test the single functions, like creating a room, joining a room, sending a message, etc.
To use one of those methods, check the source code inside the client_app.erl file, from there you can see the exported methods you can use for testing purposes.