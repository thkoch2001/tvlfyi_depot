#! /usr/bin/env -S perl -ln
use strict;

if (/^evaluating file '(.*)'$/ or
    /^copied source '(.*)' -> '.*'$/ or
    /^trace: depot-scan '(.*)'$/) {
    print $1;
    next;
}

print STDERR unless /^instantiated '.*' -> '.*'$/;
