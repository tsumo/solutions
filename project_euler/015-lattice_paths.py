#!/usr/bin/env python3

"""
Problem 15

Starting in the top left corner of a 2×2 grid, and only being able to move to
the right and down, there are exactly 6 routes to the bottom right corner.

┍━━━┯━━━┓    ┍━━━┱   ┐    ┍━━━┱   ┐
        ┃        ┃            ┃
├   ┼   ┨    ├   ╄━━━┪    ├   ╂   ┤
        ┃            ┃        ┃
└   ┴   ┚    └   ┴   ┚    └   ┺━━━┙

┎   ┬   ┐    ┎   ┬   ┐    ┎   ┬   ┐
┃            ┃            ┃
┡━━━┿━━━┪    ┡━━━╅   ┤    ┠   ┼   ┤
        ┃        ┃        ┃
└   ┴   ┚    └   ┺━━━┙    ┗━━━┷━━━┙

How many such routes are there through a 20×20 grid?
"""


import pe_utils
from math import factorial as fac


def problem15_recur1(n):
    """
    Recursively explore all possible paths.
    Can print paths to the screen.
    """
    def recur(n, x, y, path, res):
        if x != n:
            res += recur(n, x+1, y, path + ["R"], 0)
        if y != n:
            res += recur(n, x, y+1, path + ["D"], 0)
        if x == n and y == n:
            # print(path)
            return 1
        return res

    return recur(n, 0, 0, [], 0)


pe_utils.test(problem15_recur1, [11], 705432)


def problem15_recur2(n):
    """
    Simpler and faster recursion version.
    """
    def recur(x, y):
        if x == 0 or y == 0:
            return 1
        return recur(x, y - 1) + recur(x - 1, y)

    return recur(n, n)


pe_utils.test(problem15_recur2, [11], 705432)


def problem15_recur_memo(n):
    """
    Recursion with memoization.
    """
    memo = {}
    def recur(x, y):
        # Early exit without dict lookup
        if x == 0 or y == 0:
            return 1
        if (x, y) in memo:
            return memo[(x, y)]
        res = recur(x, y - 1) + recur(x - 1, y)
        memo[(x, y)] = res
        return res

    return recur(n, n)


pe_utils.test(problem15_recur_memo, [11], 705432)
pe_utils.test(problem15_recur_memo, [20], 137846528820)


def problem15_iterative(n):
    """
    Translating recursive version into iterative using dynamic programming.
    Initialize a square grid where grid[n][n] will hold the number of routes
    for n. Start from the base case of the recursive version - where x and y
    both equal zero. Initialize top and left borders with 1's (because if we
    have a one-dimension grid there will only ever be one possible route
    through it). Go through grid, calculating and storing the results on the
    way.

    Solution from the problem overview on project euler.
    """
    n += 1
    grid = [[0] * n] * n
    for i in range(n):
        grid[i][0] = 1
        grid[0][i] = 1
    for i in range(1, n):
        for j in range(1, n):
            grid[i][j] = grid[i-1][j] + grid[i][j-1]
    return grid[n-1][n-1]


pe_utils.test(problem15_iterative, [11], 705432)
pe_utils.test(problem15_iterative, [20], 137846528820)


def problem15_binomial(n):
    """
    In how many ways can we arrange 20 steps right and 20 steps down in 40
    total steps?
    The answer is binomial coefficient (40 / 20).
    See also: https://en.wikipedia.org/wiki/Catalan_number
    """
    return fac(n * 2) // (fac(n) ** 2)


pe_utils.test(problem15_binomial, [11], 705432)
pe_utils.test(problem15_binomial, [20], 137846528820)


def problem15_combinatorial(n):
    """
    In a grid of size m by n, no matter what path we take, there will be
    exactly m movements to the right and n movements down. This means that
    pathway can be represented as a string of R's and D's of legth m+n.

    Number of ways of choosing k items from a group of n items where order of
    the k items does not matter:
        n! / (k!(n-k)!)
    Using this formula we can calculate binomial ((m+n) / m)
    or rather (2n / n) in the case of the square grid.

    Solution from the problem overview on project euler.
    """
    result = 1
    for i in range(1, n+1):
        result *= (n + i) / i
    return round(result)


pe_utils.test(problem15_combinatorial, [20], 137846528820)


# One promising cute solution was found with the help of the OEIS.
# A071976 - Number of n-digit numbers with digit sum n.
# Sadly it diverges from the actual solution sequence - A000984.

# A071976     A000984     Difference
# 1           1           0
# 2           2           0
# 6           6           0
# 20          20          0
# 70          70          0
# 252         252         0
# 924         924         0
# 3432        3432        0
# 12870       12870       0
# 48619       48620       1
# 184735      184756      21
# 705222      705432      210
# 2702609     2704156     1547
# 10390940    10400600    9660
# 40062132    40116600    54468

