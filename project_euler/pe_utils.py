#!/usr/bin/env python3

import time
from datetime import timedelta
from functools import reduce


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
        print(RED, func.__name__, END)
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
    """Quickly computes sum of all integers from 1 to n."""
    return n * (n + 1) / 2


def quick_sum_of_squares(n):
    """Quickly computes sum of squares of all integers from 1 to n."""
    return (n / 6) * (2 * n + 1) * (n + 1)

