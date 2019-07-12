#!/usr/bin/env python3

import time
from datetime import timedelta
from functools import reduce
import math


def test(func, arg, expectation):
    """
    Call function, check correctness,
    time its execution and print results.
    """
    GREEN = "\033[92m"
    RED = "\033[0;31m"
    END = "\033[0m"
    t1 = time.process_time()
    res = func(*arg)
    t2 = time.process_time() - t1
    if res == expectation:
        print(GREEN, func.__name__, END, "for", arg)
    else:
        print(RED, func.__name__, END, "for", arg)
    print("    ", rpad(res), "vs", lpad(expectation), "time:", timedelta(seconds=t2))
    print()


def rpad(i, n = 10):
    """
    Right-padded string.
    """
    return str(i).rjust(n)


def lpad(i, n = 10):
    """
    Left-padded string.
    """
    return str(i).ljust(n)


def gcd(x, y):
    """Find greatest common denominator."""
    while y:
        x, y = y, x % y
    return x


def gcd_list(lst):
    """Find gcd for any number of integers."""
    return reduce(gcd, lst)


def lcm(x, y):
    """Find lowest common multiple."""
    return (x * y) // gcd(x, y)


def lcm_list(lst):
    """Find lcm for any number of integers."""
    return reduce(lcm, lst)


def quick_sum(n):
    """
    Quickly computes sum of all integers from 1 to n.

    1 + 2 + 3 = 6

    o
    o o
    o o o

    (3 * 4) / 2 = 6

    ┌──4──┐
    o x x x ┐
    o o x x 3
    o o o x ┘
    """
    return n * (n + 1) / 2


def quick_sum_of_squares(n):
    """Quickly computes sum of squares of all integers from 1 to n."""
    return (n / 6) * (2 * n + 1) * (n + 1)


def sieve_of_eratosthenes(n):
    """
    Finds all prime numbers up to a given limit.
    Works pretty fast up to a 10 000 000.
    """
    primes = []
    non_primes = set()
    for i in range(2, n + 1):
        if i not in non_primes:
            primes.append(i)
            for j in range(i, n + 1, i):
                non_primes.add(j)
    return primes


def divisors(n):
    """
    Finds all divisors of the natural number.

    Divisors are alway present in pairs of (n, x/n).
    For example, when x = 100:
        (1, 100), (2, 50), (4, 25), (5, 20), (10, 10)

    That means that we only have to go up to sqrt(x) to find all pairs of
    divisors.
    """
    divisors = set()
    for i in range(1, int(math.sqrt(n)) + 1):
        if n % i == 0:
            divisors.add(i)
            divisors.add(n / i)
    return divisors

