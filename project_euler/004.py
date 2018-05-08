#!/usr/bin/env python3

# Largest palindrome product
# Problem 4

# A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

# Find the largest palindrome made from the product of two 3-digit numbers.


#=====================


import time
from datetime import timedelta


def lp(n):
    max_number = int("9" * n)
    largest = 0
    for i in range(max_number, 0, -1):
        for j in range(max_number, 0, -1):
            k = i * j
            if is_palindrome(k) and k > largest:
                largest = k
            if k < largest:
                break
    return largest


def is_palindrome(n):
    n = str(n)
    return True if n == "".join(reversed(n)) else False


def time_and_print(func, arg, expectation):
    """
    Call function, check correctness,
    time its execution and print results.
    """
    GREEN = "\033[92m"
    RED = "\033[0;31m"
    END = "\033[0m"
    t1 = time.process_time()
    res = func(arg)
    t2 = time.process_time() - t1
    if res == expectation:
        print(GREEN, func.__name__, END, "for", arg)
    else:
        print(RED, func.__name__, END)
    print("    ", rpad(res), "vs", lpad(expectation), "time:", timedelta(seconds=t2))
    print()


def rpad(i, n = 8):
    return str(i).rjust(n)


def lpad(i, n = 8):
    return str(i).ljust(n)


time_and_print(is_palindrome, 2, True)
time_and_print(is_palindrome, 23, False)
time_and_print(is_palindrome, 232, True)
time_and_print(lp, 2, 9009)
time_and_print(lp, 3, 906609)
time_and_print(lp, 4, 99000099)

