#!/usr/bin/env python2

import sys

n = int(sys.stdin.readline().strip())

d = {}

for line in sys.stdin:
    for char in line.split()[1:]:
        if char in d:
            d[char] += 1
        else:
            d[char] = 1

for i in range(101):
    if str(i) in d:
        for _ in range(d[str(i)]):
            print(i)

