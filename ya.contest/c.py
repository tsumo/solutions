#!/usr/bin/env python3

import sys

def read_int():
    return int(sys.stdin.readline())

n = read_int()

if n < 1:
    exit()

prev = read_int()
print(prev)

for i in range(n-1):
    curr = read_int()
    if curr != prev:
        print(curr)
        prev = curr

