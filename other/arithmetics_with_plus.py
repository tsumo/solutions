#!/usr/bin/env python3

"""
Implement * - / using only +
"""

def mult(a, b):
    """ Works only for positive numbers and zero """
    c = 0
    for i in range(b):
        c += a
    return c

def sub(a, b):
    """ Works only when a >= b """
    c = 0
    while a != b:
        b += 1
        c += 1
    return c

def div(a, b):
    """ Integer division. Works only when a >= b """
    c = 0
    while a >= b:
        a = sub(a, b)
        c += 1
    return c

assert mult(3, 4) == 3 * 4
assert mult(8, 0) == 8 * 0
assert sub(9, 5) == 9 - 5
assert sub(5, 5) == 5 - 5
assert div(10, 2) == 10 // 2
assert div(9, 2) == 9 // 2
assert div(8, 2) == 8 // 2
assert div(5, 5) == 5 // 5

