provider "aws" {
  region = "eu-west-1"
}

data "aws_vpc" "primary_vpc" {
  default = true
}

data "aws_subnets" "available_subnets" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.primary_vpc.id]
  }
}

resource "aws_security_group" "erlang_chat_server_security_group" {
  name        = "erlang-chat-server-security-group"
  description = "Security group for Erlang chat server"
  vpc_id      = data.aws_vpc.primary_vpc.id

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

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "erlang-chat-server-security-group"
  }
}

resource "aws_security_group" "erlang_chat_server_load_balancer_security_group" {
  name        = "erlang-chat-server-lb-security-group"
  description = "Security group for Erlang chat server load balancer"
  vpc_id      = data.aws_vpc.primary_vpc.id

  ingress {
    from_port   = 5555
    to_port     = 5555
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "Chat server port"
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

data "aws_ami" "amazon_linux_2_ami" {
  most_recent = true
  owners      = ["amazon"]
  filter {
    name   = "name"
    values = ["amzn2-ami-hvm-*-x86_64-gp2"]
  }
}

resource "aws_launch_template" "erlang_chat_server_launch_template" {
  name_prefix   = "erlang-chat-server-launch-"
  image_id      = data.aws_ami.amazon_linux_2_ami.id
  instance_type = "t2.micro"

  network_interfaces {
    associate_public_ip_address = true
    security_groups             = [aws_security_group.erlang_chat_server_security_group.id]
  }

  key_name = "server-key-pair"

  user_data = base64encode(<<-EOF
              #!/bin/bash
              # Update system
              yum update -y
              # Install Erlang
              yum install -y wget
              wget https://packages.erlang-solutions.com/erlang-solutions-2.0-1.noarch.rpm
              rpm -Uvh erlang-solutions-2.0-1.noarch.rpm
              yum install -y erlang
              EOF
  )

  tag_specifications {
    resource_type = "instance"
    tags = {
      Name = "erlang-chat-server-instance"
    }
  }
}

resource "aws_autoscaling_group" "erlang_chat_server_autoscaling_group" {
  desired_capacity    = 2
  max_size            = 4
  min_size            = 1
  target_group_arns   = [aws_lb_target_group.erlang_chat_server_target_group.arn]
  vpc_zone_identifier = data.aws_subnets.available_subnets.ids

  launch_template {
    id      = aws_launch_template.erlang_chat_server_launch_template.id
    version = "$Latest"
  }

  tag {
    key                 = "Name"
    value               = "erlang-chat-server-asg"
    propagate_at_launch = true
  }
}

resource "aws_lb" "erlang_chat_server_network_load_balancer" {
  name               = "erlang-chat-server-nlb"
  internal           = false
  load_balancer_type = "network"
  subnets            = data.aws_subnets.available_subnets.ids

  enable_deletion_protection = false

  tags = {
    Name = "erlang-chat-server-network-load-balancer"
  }
}

resource "aws_lb_target_group" "erlang_chat_server_target_group" {
  name     = "erlang-chat-server-target-group"
  port     = 5555
  protocol = "TCP"
  vpc_id   = data.aws_vpc.primary_vpc.id

  health_check {
    enabled             = true
    port                = 5555
    protocol            = "TCP"
    healthy_threshold   = 3
    unhealthy_threshold = 3
  }
}

resource "aws_lb_listener" "erlang_chat_server_listener" {
  load_balancer_arn = aws_lb.erlang_chat_server_network_load_balancer.arn
  port              = 5555
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.erlang_chat_server_target_group.arn
  }
}

output "load_balancer_dns" {
  value       = aws_lb.erlang_chat_server_network_load_balancer.dns_name
  description = "DNS name of the Erlang chat server load balancer"
}

output "connection_command" {
  value       = "nc ${aws_lb.erlang_chat_server_network_load_balancer.dns_name} 5555"
  description = "Command to connect to the Erlang chat server via load balancer"
}
