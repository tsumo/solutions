#!/usr/bin/env python3

"""
Sort an array so that all anagrams are next to each other.
"""

import anagrams

assert anagrams.anagrams('anagram', 'nagaram') == True

lst = ['645', 'abc', '123', '456', '789', 'cba', '312', '978']

i = 1
while i < len(lst)-1:
    skips = 0
    for j in range(i, len(lst)):
        if anagrams.anagrams(lst[j], lst[i-1]):
            lst[j], lst[i] = lst[i], lst[j]
            skips += 1
    i += 1
    i += skips

print(lst)

