#!/usr/bin/env python2

# Source:
# https://sahandsaba.com/interview-question-generating-all-balanced-parentheses.html

import sys

n = int(sys.stdin.readline())

def gen_balanced_iterative(n):
    # Start with the lexicographically smallest string.
    s = ['('] * n + [')'] * n
    while True:
        yield ''.join(s)
        o, c = 0, 0  # Opening and closing parentheses count
        for i in range(1, 2 * n + 1):
            # If we are checking the very first character in the string we are
            # definitely done since we can not possibly change the first
            # character from ( to ).
            if i == 2 * n:
                return
            if s[-i] == '(':
                o += 1
                if c > o:
                    # This is our opportunity to change '(' to ')' at index i
                    # and then replace the rest of the string with the smallest
                    # lexicographic suffix, which is o opening and c - 1
                    # closing parentheses (we already placed one closing
                    # parenthesis in the string so c - 1 left to place).
                    s[-i:] = [')'] + ['('] * o + [')'] * (c - 1)
                    break
            else:
                c += 1

for p in gen_balanced_iterative(n):
    print(p)

