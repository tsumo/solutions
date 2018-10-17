#!/usr/bin/env python3

"""
Memory: O(1)
    Sorts array in place

Time: O(n^2)

"""

import math
import sorting_utils

random_list = sorting_utils.random_list(50, 1000000)

def sel_sort(lst):
    for i in range(len(lst)):           # Go through whole list
        min_index = i                   # Assume that current element in min
        for j in range(i + 1, len(lst)):    # Go through the rest of list
            if lst[min_index] > lst[j]:     # Find new minimum
                min_index = j
        if min_index != i:              # Swap current with min if needed
            lst[i], lst[min_index] = lst[min_index], lst[i]
    return lst


print(sel_sort(random_list))

