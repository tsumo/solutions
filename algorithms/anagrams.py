#!/usr/bin/env python3

"""
Determine if two strings are anagrams.
"""

from collections import defaultdict

def anagrams(s1, s2):
    d = defaultdict(int)
    for c in s1:
        d[c] += 1
    for c in s2:
        d[c] -= 1
    for k in d:
        if d[k] != 0:
            return False
    return True

print(anagrams('anagram', 'nagaram'))
print(anagrams('qwerty', 'qwertu'))

