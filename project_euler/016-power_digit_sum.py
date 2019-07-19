#!/usr/bin/env python3

"""
Promblem 16

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
"""


import pe_utils


def problem16(n):
    return pe_utils.digit_sum(2 ** n)


pe_utils.test(problem16, [15], 26)
pe_utils.test(problem16, [1000], 1366)

