Simple Chat App written in Erlang
=====

A simple chat application written in `Erlang` that allows users to create, join, and manage chat rooms. Users can also send private messages and create private rooms with controlled access.

## Features

- Create and manage public and private chat rooms
- Send messages in rooms or privately to other users
- List available rooms and users
- Invite users to private chat rooms

The server automatically starts when the application is started, when you open a rebar3 shell you don't have to start it anymore. 

When the client connects (with a tcp client like `telnet` or `netcat`) it can send and HELP command to get the list of available commands:

```
CREATE <room_name> - Create a new chat room
DESTROY <room_name> - Destroy a room (creator only)
LIST - Show all available rooms
JOIN <room_name> - Join an existing room
LEAVE <room_name> - Leave a room
MSG <room_name> <message> - Send a message to a room
HELP - Show this help message
PRIVATE <user> <msg> - Send private message to user,
USERS - Show all users,
CREATE PRIVATE <room_name> - Create a new private chat room,
INVITE <room_name> <username> - Invite a user to a private room,
```

## Terraform

the project also contains a terraform file to deploy the application on AWS with load-balancing included.
