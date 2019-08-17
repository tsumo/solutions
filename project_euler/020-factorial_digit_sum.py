#!/usr/bin/env python3

"""
Problem 20

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
"""


import pe_utils
from math import factorial as fac


def problem20(n):
    return sum([int(x) for x in str(fac(n))])

pe_utils.test(problem20, [10], 27)
pe_utils.test(problem20, [100], 648)

