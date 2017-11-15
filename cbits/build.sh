#!/bin/bash

rm test.o
cc -fPIC -c test.c
ar rc libtest.a test.o
cc -shared -o libtest.so test.o
