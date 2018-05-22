#!/usr/bin/env python3

"""
Sum square difference
Problem 6

The sum of the squares of the first ten natural numbers is,
1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.
"""


import time
from datetime import timedelta


def problem6_slow(n):
    lst = range(1, n + 1)
    sum_of_the_squares = sum(map(lambda x: x ** 2, lst))
    square_of_the_sum = sum(lst) ** 2
    return square_of_the_sum - sum_of_the_squares


def problem6(n):
    sum_of_the_squares = quick_sum_of_squares(n)
    square_of_the_sum = quick_sum(n) ** 2
    return square_of_the_sum - sum_of_the_squares


def quick_sum(n):
    """Quickly computes sum of all integers from 1 to n."""
    return n * (n + 1) / 2


def quick_sum_of_squares(n):
    """Quickly computes sum of squares of all integers from 1 to n."""
    return (n / 6) * (2 * n + 1) * (n + 1)


def time_and_print(func, arg, expectation):
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
        print(GREEN, func.__name__, END, "for", *arg)
    else:
        print(RED, func.__name__, END, "for", *arg)
    print("    ", rpad(res), "vs", lpad(expectation), "time:", timedelta(seconds=t2))
    print()


def rpad(i, n = 8):
    return str(i).rjust(n)


def lpad(i, n = 8):
    return str(i).ljust(n)


time_and_print(problem6_slow, [10], 2640)
time_and_print(problem6_slow, [100], 25164150)
time_and_print(problem6_slow, [1000], 250166416500)
time_and_print(problem6_slow, [10000], 2500166641665000)
time_and_print(problem6, [4234820398492084], 80404433063647405498749603128266393886681918708568065071644672)

