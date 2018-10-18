#!/usr/bin/env python3

"""
Rotate a square matrix by 90 degrees in place.
"""

def rotate(m):
    l = len(m)
    for i in range(l):
        for j in range(l - i):
            x = i
            y = i + j
            m[x][y], m[y][x] = m[y][x], m[x][y]
    return m

print(rotate([ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]))

