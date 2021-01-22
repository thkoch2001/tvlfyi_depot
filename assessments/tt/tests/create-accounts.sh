#!/usr/bin/env sh

# This script populates the Accounts table over HTTP.

http POST :3000/accounts \
  username=mimi \
  password=testing \
  email=miriamwright@google.com \
  role=user

http POST :3000/accounts \
  username=bill \
  password=testing \
  email=wpcarro@gmail.com \
  role=manager

http POST :3000/accounts \
  username=wpcarro \
  password=testing \
  email=wpcarro@google.com \
  role=admin
