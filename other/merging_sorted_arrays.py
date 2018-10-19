#!/usr/bin/env python3

"""
Merge sorted arrays A and B, where A has large enough buffer
at the end to hold B.
"""

import random

A = sorted([random.randrange(100) for _ in range(10)])
B = sorted([random.randrange(100) for _ in range(10)])
A += [None] * 10

i = len(B) - 1      # Index for A
j = len(B) - 1      # Index for B
k = len(A) - 1      # Index for insertion into A

while k >= 0:
    if i >= 0 and A[i] >= B[j]:
        A[k] = A[i]
        i -= 1
    if j >= 0 and B[j] > A[i]:
        A[k] = B[j]
        j -= 1
    k -= 1

print(A)

