#!/usr/bin/env python3

"""
Problem 23

A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
"""


import pe_utils


limit = 28123


def problem23():
    _, _, abundant = pe_utils.perfect_numbers(limit)
    # Trade memory for lookup speed
    ab_dict = {i: i in abundant for i in range(limit + 1)}
    # Stores numbers that CAN be written as sum of two abundant numbers
    result_dict = {}
    for i, x in enumerate(abundant):
        for y in abundant[i:]:
            xy = x + y
            # Don't need numbers over limit
            if (xy <= limit):
                result_dict[xy] = True
    result = 0
    for i in range(limit + 1):
        if i not in result_dict:
            result += i
    return result


pe_utils.test(problem23, [], 4179871)

