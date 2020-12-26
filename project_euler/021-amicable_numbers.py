#!/usr/bin/env python3

"""
Problem 21

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
"""


import pe_utils


def problem21(n):
    nums = set()
    div_sums = {}

    # Precalculate all sums of divisors
    for i in range(2, n):
        div_sums[i] = sum(pe_utils.proper_divisors(i))

    for i in range(2, n):
        # Start inner loop from i
        for j in range(i, n):
            if i == j:
                continue
            if (div_sums[i] == j and div_sums[j] == i):
                nums.add(i)
                nums.add(j)
    return sum(nums)


pe_utils.test(pe_utils.proper_divisors, [220], {1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110})
pe_utils.test(pe_utils.proper_divisors, [284], {1, 2, 4, 71, 142})
pe_utils.test(problem21, [300], 504)
pe_utils.test(problem21, [10000], 31626)

