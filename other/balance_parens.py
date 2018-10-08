#!/usr/bin/env python3

"""
Find if a string of parenthesis are valid and balanced.
"""

import collections, queue

a = queue.Queue()

test_strings = [
    '()((()()))(()((())))',     # balanced
    ')((()()))(()((())))',      # unbalanced
    '(()((()()))(()((())))',    # unbalanced
    '(({[]}{()})[])',           # balanced
    '(([]}{()})[])',            # unbalanced
    '(({[]}{()})[)',            # unbalanced
]

def use_counters(parens):
    pairs = {')': '(', ']': '[', '}': '{'}
    c = collections.defaultdict(int)
    for x in parens:
        if x in pairs.values():
            c[x] += 1
        elif x in pairs.keys():
            c[pairs[x]] -= 1
        # return early from obvious imbalances
        for v in c.values():
            if v < 0:
                return 'unbalanced'
    for v in c.values():
        if v != 0:
            return 'unbalanced'
    return 'balanced'

def use_queue(parens):
    pairs = {')': '(', ']': '[', '}': '{'}
    q = queue.LifoQueue()
    for x in parens:
        if x in pairs.values():
            q.put(x)
        elif x in pairs.keys():
            # closing paren should not occur when queue is empty
            # closing parens should match most recent opening paren
            if q.empty() or q.get() != pairs[x]:
                return 'unbalanced'
    return 'balanced' if q.empty() else 'unbalanced'

def test(test_string, test_func):
    print(test_func.__name__, '-', test_string, test_func(test_string))

for test_string in test_strings:
    test(test_string, use_counters)
print()
for test_string in test_strings:
    test(test_string, use_queue)

