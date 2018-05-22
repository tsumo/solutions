#!/usr/bin/env python3

"""
Smallest multiple
Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
"""


import time
from datetime import timedelta
from functools import reduce


def problem5(n):
    """Computes lowest common multiple for integers from 1 to n."""
    return lcm_list(range(1, n + 1))


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


time_and_print(gcd, [42, 56], 14)
time_and_print(gcd, [54, 24], 6)

time_and_print(gcd_list, [(1, 2, 3)], 1)
time_and_print(gcd_list, [(2, 4, 6, 8)], 2)

time_and_print(lcm, [5, 2], 10)

time_and_print(lcm_list, [(5, 2, 1)], 10)
time_and_print(lcm_list, [(2, 7, 3, 9, 4)], 252)

time_and_print(problem5, [1], 1)
time_and_print(problem5, [2], 2)
time_and_print(problem5, [3], 6)
time_and_print(problem5, [4], 12)
time_and_print(problem5, [5], 60)
time_and_print(problem5, [6], 60)
time_and_print(problem5, [7], 420)
time_and_print(problem5, [8], 840)
time_and_print(problem5, [9], 2520)
time_and_print(problem5, [10], 2520)
time_and_print(problem5, [11], 27720)
time_and_print(problem5, [12], 27720)
time_and_print(problem5, [13], 360360)
time_and_print(problem5, [14], 360360)
time_and_print(problem5, [15], 360360)
time_and_print(problem5, [16], 720720)
time_and_print(problem5, [17], 12252240)
time_and_print(problem5, [18], 12252240)
time_and_print(problem5, [19], 232792560)
time_and_print(problem5, [20], 232792560)
time_and_print(problem5, [100], 69720375229712477164533808935312303556800)
time_and_print(problem5, [216], 71168947243747441838927276793711536968235214767668176422286721624536377810802929304135312000)
time_and_print(problem5, [1000], 7128865274665093053166384155714272920668358861885893040452001991154324087581111499476444151913871586911717817019575256512980264067621009251465871004305131072686268143200196609974862745937188343705015434452523739745298963145674982128236956232823794011068809262317708861979540791247754558049326475737829923352751796735248042463638051137034331214781746850878453485678021888075373249921995672056932029099390891687487672697950931603520000)

