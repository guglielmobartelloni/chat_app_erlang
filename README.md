chat_app_miniclip
=====

# Question 2

To test the server functionality open a shell:

```bash
rebar3 shell
```

and start the server with the command:

```erl
chat_app_miniclip:start(5555)
```

`5555` is the port number where the server will listen. 
Open another shell and connect the client to the server with the command:

```erl
chat_client:start("localhost", 5555)
```
now you can send messages from the client by entering the text and hitting enter. The server will display the received messages

# Question 3

Now the server automatically starts when the application is started, when you open a rebar3 shell you don't have to start it anymore. 

When the client connects it can send and HELP command to get the list of available commands:

```
CREATE <room_name> - Create a new chat room
DESTROY <room_name> - Destroy a room (creator only)
LIST - Show all available rooms
JOIN <room_name> - Join an existing room
LEAVE <room_name> - Leave a room
MSG <room_name> <message> - Send a message to a room
HELP - Show this help message
```

the format is clear explains the commands, the client can use any of the commands as specified to manage rooms and send messages.

# Question 4

In the `terraform/` folder there is the `main.tf` file that let's you spin up an EC2 with Erlang installed.

# Question 6

This question adds two commands:

```
"PRIVATE <user> <msg> - Send private message to user",
"USERS - Show all users",
```

the username will be a random username generated when the clients connect.
