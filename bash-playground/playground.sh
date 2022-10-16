#!/bin/bash

var1=A
var2=B

my_func(){
  echo $1
  return $(($1>10))
}

my_func 11 
my_func 1

echo $?