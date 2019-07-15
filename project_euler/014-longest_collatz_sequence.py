#!/usr/bin/env python3

"""
Problem 14

The following iterative sequence is defined for the set of positive integers:

    n -> n/2 (n is even)
    n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
    13 - 40 - 20 - 10 - 5 - 16 - 8 - 4 - 2 - 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains
10 terms. Although it has not been proved yet (Collatz Problem), it is thought
that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
"""


import pe_utils


def collatz_seq_generator(x):
    while x != 1:
        yield x
        if x % 2 == 0:
            x = x // 2
        else:
            x = 3 * x + 1
    yield x


collatz_seq = collatz_seq_generator(13)
pe_utils.test(next, [collatz_seq], 13)
pe_utils.test(next, [collatz_seq], 40)
pe_utils.test(next, [collatz_seq], 20)
pe_utils.test(next, [collatz_seq], 10)
pe_utils.test(next, [collatz_seq], 5)
pe_utils.test(next, [collatz_seq], 16)
pe_utils.test(next, [collatz_seq], 8)
pe_utils.test(next, [collatz_seq], 4)
pe_utils.test(next, [collatz_seq], 2)
pe_utils.test(next, [collatz_seq], 1)


"""
First we try to brute force the solution.
"""
def collatz_length_naive(x):
    collatz_seq = collatz_seq_generator(x)
    return sum(1 for _ in collatz_seq)


pe_utils.test(collatz_length_naive, [525], 31)


"""
Add memoization. This alone gives ~10-30x speedup.
"""
lengths = {}
def collatz_length_memo(x):
    if x in lengths:
        return lengths[x]
    if x == 1:
        return 1
    if x % 2 == 0:
        lengths[x] = 1 + collatz_length_memo(x // 2)
    else:
        # When n is odd, 3n+1 will be even
        # therefore next numbers will be:
        #   n -> 3n+1 -> ((3n+1))/2
        # therefore:
        #   length(n) = length((3n+1)/2)+2
        lengths[x] = 2 + collatz_length_memo((3 * x + 1) // 2)
    return lengths[x]


pe_utils.test(collatz_length_memo, [525], 31)


def problem14(upper_bound, length_func):
    max_length = 0
    result = upper_bound

    # When n is even next numbers will be
    #   n -> n/2
    # therefore:
    #   length(n) = length(n/2)+1
    # therefore:
    #   length(2n) > length(n)
    # That means we don't need to check for numbers below n/2
    # We can check just 500 000 - 1 000 000
    # instead of 0 - 1 000 000
    lower_bound = 0
    if upper_bound % 2 == 0:
        lower_bound = upper_bound // 2

    for i in range(lower_bound, upper_bound):
        seq_length = length_func(i)
        if seq_length > max_length:
            max_length = seq_length
            result = i
    return result


pe_utils.test(problem14, [1000000, collatz_length_naive], 837799)
pe_utils.test(problem14, [1000000, collatz_length_memo], 837799)

