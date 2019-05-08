#!/usr/bin/env python3

import sys
from collections import defaultdict

freq_first = defaultdict(lambda: 0)
freq_second = defaultdict(lambda: 0)
in_first = True

for char in sys.stdin.read():
    if char == '\n':
        in_first = False;
        continue
    if in_first:
        freq_first[char] += 1
    else:
        freq_second[char] += 1

if len(freq_first) != len(freq_second):
    print(0)
    exit()

for char, count in freq_first.items():
    if freq_second[char] != count:
        print(0)
        exit()

print(1)

