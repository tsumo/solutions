#!/usr/bin/env python3

"""
Smallest multiple
Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
"""


import pe_utils


def problem5(n):
    """Computes lowest common multiple for integers from 1 to n."""
    return pe_utils.lcm_list(range(1, n + 1))


pe_utils.test(problem5, [1], 1)
pe_utils.test(problem5, [2], 2)
pe_utils.test(problem5, [3], 6)
pe_utils.test(problem5, [4], 12)
pe_utils.test(problem5, [5], 60)
pe_utils.test(problem5, [6], 60)
pe_utils.test(problem5, [7], 420)
pe_utils.test(problem5, [8], 840)
pe_utils.test(problem5, [9], 2520)
pe_utils.test(problem5, [10], 2520)
pe_utils.test(problem5, [11], 27720)
pe_utils.test(problem5, [12], 27720)
pe_utils.test(problem5, [13], 360360)
pe_utils.test(problem5, [14], 360360)
pe_utils.test(problem5, [15], 360360)
pe_utils.test(problem5, [16], 720720)
pe_utils.test(problem5, [17], 12252240)
pe_utils.test(problem5, [18], 12252240)
pe_utils.test(problem5, [19], 232792560)
pe_utils.test(problem5, [20], 232792560)
pe_utils.test(problem5, [100], 69720375229712477164533808935312303556800)
pe_utils.test(problem5, [216], 71168947243747441838927276793711536968235214767668176422286721624536377810802929304135312000)
pe_utils.test(problem5, [1000], 7128865274665093053166384155714272920668358861885893040452001991154324087581111499476444151913871586911717817019575256512980264067621009251465871004305131072686268143200196609974862745937188343705015434452523739745298963145674982128236956232823794011068809262317708861979540791247754558049326475737829923352751796735248042463638051137034331214781746850878453485678021888075373249921995672056932029099390891687487672697950931603520000)

