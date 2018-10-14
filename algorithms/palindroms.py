#!/usr/bin/env python3

"""
Determine if string is a palindrome
"""

def palindrome(s):
    if len(s) <= 1:
        return True
    if s[:1] == s[-1]:
        return palindrome(s[1:-1])
    return False

print(palindrome('abcdedcba'))
print(palindrome('zbcdedcba'))

