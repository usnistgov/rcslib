#!/bin/sh

find . '(' -name \*.cc -o -name \*.hh -o -name \*.h -o -name \*.c -o -name \*.java ')' -a -not -exec grep "title 17 Section 105" '{}' \; -exec  ./add_header.sh '{}' \;

