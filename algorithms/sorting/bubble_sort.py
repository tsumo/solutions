#!/usr/bin/env python3

"""
Memory: O(1)

Time: O(n^2)

Not a practical algorithm.
"""

import sorting_utils

random_list = sorting_utils.random_list(50, 1000000)

def bubble(lst):
    length = len(lst)
    # Go through the entire list
    for i in range(length):
        # Go from list end down to i
        for j in range(length-1, i, -1):
            # Swap if neighbors are out of order
            if lst[j] < lst[j-1]:
                lst[j], lst[j-1] = lst[j-1], lst[j]
    return lst

print(bubble(random_list))

