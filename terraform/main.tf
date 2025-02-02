provider "aws" {
  region = "eu-west-1"
}

data "aws_vpc" "default" {
  default = true
}

resource "aws_security_group" "chat_server_sg" {
  name        = "chat-server-sg"
  description = "Security group for Erlang chat server"
  vpc_id      = data.aws_vpc.default.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "SSH access"
  }

  ingress {
    from_port   = 5555
    to_port     = 5555
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "Chat server port"
  }

  # Allow all outbound traffic
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "chat-server-sg"
  }
}

data "aws_ami" "amazon_linux_2" {
  most_recent = true
  owners      = ["amazon"]

  filter {
    name   = "name"
    values = ["amzn2-ami-hvm-*-x86_64-gp2"]
  }
}

# EC2 Instance
resource "aws_instance" "chat_server" {
  ami           = data.aws_ami.amazon_linux_2.id
  instance_type = "t2.micro"

  vpc_security_group_ids = [aws_security_group.chat_server_sg.id]
  
  key_name = "server-key-pair"

  user_data = <<-EOF
              #!/bin/bash
              # Update system
              yum update -y

              # Install Erlang
              yum install -y wget
              wget https://packages.erlang-solutions.com/erlang-solutions-2.0-1.noarch.rpm
              rpm -Uvh erlang-solutions-2.0-1.noarch.rpm
              yum install -y erlang
              EOF

  tags = {
    Name = "erlang-chat-server"
  }
}

output "chat_server_public_ip" {
  value = aws_instance.chat_server.public_ip
  description = "Public IP address of the chat server"
}

output "connection_command" {
  value = "nc ${aws_instance.chat_server.public_ip} 5555"
  description = "Command to connect to the chat server"
}
