#!/usr/bin/env python3

"""
Memory: O(1)
    Modifies array in place,
    can sort while receiving array.

Time: O(n^2)
    May have to check every element,
    swap every element with all others.

Effective for already nearly sorted arrays.
In this case time complexity can be closer to O(n)
"""

import sorting_utils

random_list = sorting_utils.random_list(50, 1000000)

def ins_sort(lst):
    # Starting from the second element
    for j in range(1, len(lst)):
        key = lst[j]            # Remember current element
        i = j - 1               # Starting from the previous element
        while i >= 0 and lst[i] > key:  # While numbers are bigger
            lst[i+1] = lst[i]   # Move them to the right
            i -= 1
        lst[i+1] = key          # Insert current element at the
                                # new position
    # Head of the list gets progressively sorted
    return lst

def ins_sort_rev(lst):
    # Reverse sorting
    for j in range(1, len(lst)):
        key = lst[j]
        i = j - 1
        while i >= 0 and lst[i] < key:  # The only difference
            lst[i+1] = lst[i]           # is in the comparison
            i -= 1
        lst[i+1] = key
    return lst

print(ins_sort(random_list))
print(ins_sort_rev(random_list))

