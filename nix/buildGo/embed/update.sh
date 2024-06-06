#!/usr/bin/env bash

set -ex

curl -o read.go https://raw.githubusercontent.com/bazelbuild/rules_go/master/go/tools/builders/read.go
curl -o filter.go https://raw.githubusercontent.com/bazelbuild/rules_go/master/go/tools/builders/filter.go
curl -o embedcfg.go https://raw.githubusercontent.com/bazelbuild/rules_go/master/go/tools/builders/embedcfg.go
