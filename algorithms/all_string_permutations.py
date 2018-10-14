#!/usr/bin/env python3

"""
Return all possible permutations of a string.
For 'abc':
    'abc', 'acb', 'bac', 'bca', 'cab', 'cba'

Chop off the last character of the string.
Generate all possible permutations of resulting substring.
Insert last character into every possible place in the resulting
perumtations.
merge('ab', 'C') = 'Cab', 'aCb', 'abC'
merge('ba', 'C') = 'Cba', 'bCa', 'baC'
"""

def perm(string):
    if len(string) == 1:        # Base case
        return string
    merge_char = string[-1]     # Last character
    permutations = perm(string[:-1])    # Al permutations for substring
    res = []
    for permutation in permutations:
        res.extend(merge(permutation, merge_char))
    return res

def merge(string, merge_char):
    res = []
    for i in range(len(string)+1):      # Insert character in every possible
        res.append(string[:i] + merge_char + string[i:])    # place
    return res

print(perm('abcde'))

