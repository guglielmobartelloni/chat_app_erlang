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

# Question 4

In the `terraform/` folder there is the `main.tf` file that let's you spin up an EC2 with Erlang installed.
