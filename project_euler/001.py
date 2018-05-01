#!/usr/bin/env python3

# Multiples of 3 and 5
# Problem 1

# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

# Find the sum of all the multiples of 3 or 5 below 1000.


#=====================


import time


def brute():
    """
    Brute force approach.
    Go through every number and check it.
    """
    s = 0
    for i in range(1000):
        if i % 3 == 0 or i % 5 == 0:
            s += i
    return s


def sets():
    """
    Use multiplication to get only needed numbers.
    Use set properties to get rid of multiples.
    """
    s = set()
    i = 0
    while i < 1000:
        s.add(i)
        i += 3
    i = 0
    while i < 1000:
        s.add(i)
        i += 5
    return sum(s)


def time_and_print(func):
    """
    Call function, time its execution
    and print results.
    """
    GREEN = "\033[92m"
    RED = "\033[0;31m"
    END = "\033[0m"
    t1 = time.process_time()
    res = func()
    t2 = time.process_time() - t1
    print(func.__name__)
    print("    Result:", GREEN, res, END, "time:", t2)


time_and_print(brute)
time_and_print(sets)

