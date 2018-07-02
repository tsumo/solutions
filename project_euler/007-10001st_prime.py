#!/usr/bin/env python3

"""
10001st prime
Problem 7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
we can see that the 6th prime is 13.

What is the 10 001st prime number?
"""


import pe_utils
from math import log


def problem7(n):
    """
    It has been proven that nth prime lies between
    n * ln(n) + n * ln(ln(n) - 1) and n * ln(n) + n * ln(ln(n))
    for n >= 6. This gives us an upper limit for the sieve.
    """
    if n >= 6:
        upper_limit = int(n * log(n) + n * log(log(n)))
        return pe_utils.sieve_of_eratosthenes(upper_limit)[n - 1]
    else:
        return pe_utils.sieve_of_eratosthenes(13)[n - 1]


pe_utils.test(problem7, [1], 2)
pe_utils.test(problem7, [6], 13)
pe_utils.test(problem7, [10001], 104743)
pe_utils.test(problem7, [100001], 1299721)

