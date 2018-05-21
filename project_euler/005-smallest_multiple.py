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
    return int((x * y) / gcd(x, y))


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
        print(RED, func.__name__, END)
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
time_and_print(problem5, [100], 312407341776354014834072243597373389403112974556867619398435671775959632070648403268524827223839997952)

# Limit for python, anything above it triggers overflow error
time_and_print(problem5, [216], 52503908205228064815745545773254926275415249958052793967874402432056016303463623443515191694965299141695680828177190585805551209390275757682590502543120903143361355477849812695222329140533883691651398193528968197001697059419802198276640075180420531485144161679970139414339513796435983831967523325240615632896)

