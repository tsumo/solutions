#!/usr/bin/env python3

import sys

def read_int():
    return int(sys.stdin.readline().strip())

n = read_int()

in_seq = False
ones = 0
max_ones = 0

for i in range(n):
    x = read_int()
    if x != 1:
        in_seq = False
        max_ones = max(ones, max_ones)
        ones = 0
        continue
    in_seq = True
    ones += 1

print(max(ones, max_ones))

