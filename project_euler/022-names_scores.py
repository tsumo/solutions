#!/usr/bin/env python3

"""
Problem 22

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
"""


import pe_utils


def alphabetical_value(s):
    return sum([ord(c) - 64 for c in s])


def problem22():
    names_file = open('022-names_scores.txt', 'r')
    names = sorted(map(lambda x: x[1:][:-1], names_file.read().split(',')))
    names_file.close()
    result = 0
    for i, name in enumerate(names):
        result += (i + 1) * alphabetical_value(name)
    return result


pe_utils.test(alphabetical_value, ['COLIN'], 53)
pe_utils.test(problem22, [], 871198282)

