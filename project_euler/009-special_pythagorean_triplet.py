#!/usr/bin/env python3

"""
Problem 9

A Pythagorean triplet is a set of three natural numbers,
a < b < c, for which,
    a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which
a + b + c = 1000.

Find the product a * b * c.
"""


import pe_utils
from math import sqrt
from math import ceil


def problem9_naive(n):
    """
    Simple brute force approach.
    Check all a, b, c with the sum of n, with a couple of optimizations.
    Exactly one of a, b is divisible by 3.
    Exactly one of a, b is divisible by 4.
    """
    for a in range(4, n, 4):
        for b in range(3, n - a):
            c = n - a - b
            if a ** 2 + b ** 2 == c ** 2:
                return a * b * c
    return None


def problem9_complex(n):
    """
    Generating all triples using squares of the complex numbers.
    This method is lifted from the 3blue1brown video on youtube.
    https://www.youtube.com/watch?v=QJYmyhnaaek
    This method generates primes in all 4 quadrants of the graph,
    so it's a little bit inefficient.
    For large numbers it's actually slower than the brute force method.
    """
    # Start with 1 to avoid triples with zero.
    for u in range(1, n):
        for v in range(u, n):
            # When u == v one of the a, b becomes 0
            if u != v:

                # The following two blocks do the same thing.
                # First one uses complex numbers.
                # Second one uses generalised formulas.

                # comp = complex(u, v) ** 2
                # a = abs(comp.real)
                # b = abs(comp.imag)
                # c = sqrt(a ** 2 + b ** 2)

                a = abs(u ** 2 - v ** 2)
                b = abs(2 * u * v)
                c = abs(u ** 2 + v ** 2)

                if a + b + c == n:
                    return a * b * c
    return None


def problem9_fast(n):
    """
    Algorithm from the problem overview on projecteuler.net

    Every triplet can be represented as
    a = (u^2 - v^2) * d
    b = 2 * u * v * d
    c = (u^2 + v^2) * d
    where u > v > 0 and gcd(u, v) == 1 and one of u, v is even.
    d is the of the a, b, c.

    a + b + c = 2 * m * (m + n) * d
    """
    n2 = n // 2
    ulimit = ceil(sqrt(n2)) - 1
    for u in range(2, ulimit):
        if n2 % u == 0:
            nu = n2 // u
            while nu % 2 == 0:
                nu //= 2
            if u % 2 == 1:
                k = u + 2
            else:
                k = u + 1
            while k < 2 * u and k <= nu:
                if nu % k == 0 and pe_utils.gcd(k, u) == 1:
                    d = n2 // (k * u)
                    v = k - u
                    a = (u ** 2 - v ** 2) * d
                    b = 2 * u * v * d
                    c = (u ** 2 + v ** 2) * d
                    return a * b * c
                k += 2
    return None


pe_utils.test(problem9_naive, [7], None)
pe_utils.test(problem9_naive, [12], 60)
pe_utils.test(problem9_naive, [30], 780)
pe_utils.test(problem9_naive, [48], 3840)
pe_utils.test(problem9_naive, [80], 16320)
pe_utils.test(problem9_naive, [82], None)
pe_utils.test(problem9_naive, [84], 15540)
pe_utils.test(problem9_naive, [1000], 31875000)

pe_utils.test(problem9_complex, [7], None)
pe_utils.test(problem9_complex, [12], 60)
pe_utils.test(problem9_complex, [30], 780)
pe_utils.test(problem9_complex, [48], 3840)
pe_utils.test(problem9_complex, [80], 16320)
pe_utils.test(problem9_complex, [82], None)
pe_utils.test(problem9_complex, [84], 15540)
pe_utils.test(problem9_complex, [1000], 31875000)

pe_utils.test(problem9_fast, [1000], 31875000)

