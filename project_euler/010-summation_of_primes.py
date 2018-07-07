#!/usr/bin/env python3

"""
Summation of primes
Problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
"""


import pe_utils
from functools import reduce
from operator import add


def problem10(n):
    return reduce(add, pe_utils.sieve_of_eratosthenes(n))


pe_utils.test(problem10, [2000000], 142913828922)

