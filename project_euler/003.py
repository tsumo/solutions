#!/usr/bin/env python3

# Largest prime factor
# Problem 3

# The prime factors of 13195 are 5, 7, 13 and 29.

# What is the largest prime factor of the number 600851475143 ?


#=====================


import time
from datetime import timedelta


def lpf_naive(n):
    largest_prime = 0
    current_prime = 0
    while current_prime < n:
        current_prime = next_prime(current_prime)
        if n % current_prime == 0:
            n /= current_prime
            largest_prime = current_prime
    return largest_prime


def lpf_fast(n):
    i = 1
    while n > 1:
        i += 1
        while n % i == 0:
            n /= i
    return i


def lpf_faster(n):
    """
    Since 2 is the only even prime we can treat
    it separately and increase i by 2.
    """
    while n % 2 == 0:
        n /= 2
    i = 1
    while n > 1:
        i += 2
        while n % i == 0:
            n /= i
    return i


def is_prime(n):
    """
    Checks if the number is a prime.
    """
    if n == 0 or n == 1:
        return False
    for i in range(n - 1, 1, -1):
        if n % i == 0:
            return False
    return True


def next_prime(n):
    """
    Returns next prime number n + x.
    """
    while True:
        n += 1
        if is_prime(n):
            return n


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
        print(GREEN, func.__name__, END)
    else:
        print(RED, func.__name__, END)
    print("    ", rpad(res), "vs", lpad(expectation), "time:", timedelta(seconds=t2))


def rpad(i, n = 8):
    return str(i).rjust(n)


def lpad(i, n = 8):
    return str(i).ljust(n)


time_and_print(lpf_naive, 13195, 29)
# time_and_print(lpf_naive, 600851475143, 6857)
# time_and_print(lpf_naive, 600851475142, 22567)
print("-----")
time_and_print(lpf_fast, 600851475143, 6857)
time_and_print(lpf_fast, 600851475142, 22567)
time_and_print(lpf_fast, 234783486, 1863361)
time_and_print(lpf_fast, 876426, 79)
print("-----")
time_and_print(lpf_faster, 600851475143, 6857)
time_and_print(lpf_faster, 600851475142, 22567)
time_and_print(lpf_faster, 234783486, 1863361)
time_and_print(lpf_faster, 876426, 79)

