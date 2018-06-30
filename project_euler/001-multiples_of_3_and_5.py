#!/usr/bin/env python3

"""
Multiples of 3 and 5
Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we
get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
"""


import pe_utils


def brute(lim):
    """
    Brute force approach.
    Go through every number and check it.
    """
    s = 0
    for i in range(lim):
        if i % 3 == 0 or i % 5 == 0:
            s += i
    return s


def sets(lim):
    """
    Use multiplication to get only needed numbers.
    Use set properties to get rid of multiples.
    """
    s = set()
    i = 0
    while i < lim:
        s.add(i)
        i += 3
    i = 0
    while i < lim:
        s.add(i)
        i += 5
    return sum(s)


pe_utils.test(brute, [1000], 233168)
pe_utils.test(sets, [1000], 233168)

