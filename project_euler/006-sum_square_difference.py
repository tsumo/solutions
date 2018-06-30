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


import pe_utils


def problem6_slow(n):
    lst = range(1, n + 1)
    sum_of_the_squares = sum(map(lambda x: x ** 2, lst))
    square_of_the_sum = sum(lst) ** 2
    return square_of_the_sum - sum_of_the_squares


def problem6(n):
    sum_of_the_squares = pe_utils.quick_sum_of_squares(n)
    square_of_the_sum = pe_utils.quick_sum(n) ** 2
    return square_of_the_sum - sum_of_the_squares


pe_utils.test(problem6_slow, [10], 2640)
pe_utils.test(problem6_slow, [100], 25164150)
pe_utils.test(problem6_slow, [1000], 250166416500)
pe_utils.test(problem6_slow, [10000], 2500166641665000)
pe_utils.test(problem6, [4234820398492084], 80404433063647405498749603128266393886681918708568065071644672)

