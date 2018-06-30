#!/usr/bin/env python3

"""
Largest palindrome product
Problem 4

A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
"""


import pe_utils


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


pe_utils.test(is_palindrome, [2], True)
pe_utils.test(is_palindrome, [23], False)
pe_utils.test(is_palindrome, [232], True)
pe_utils.test(lp, [2], 9009)
pe_utils.test(lp, [3], 906609)
pe_utils.test(lp, [4], 99000099)

