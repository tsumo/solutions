#!/usr/bin/env python3

"""
Memory: O(n)

Time: O(n log(n))

Employs divide-and-conquer approach:
    DIVIDE sequence into the equal halves
    CONQUER by sorting both using merge sort
    MERGE sorted subsequences

Preserves input order of equal elements.
"""

import sorting_utils

random_list = sorting_utils.random_list(50, 1000000)

# Split array into two equaly sized (plus/minus 1) arrays
def divide(lst):
    half = len(lst) // 2
    return lst[:half], lst[half:]

# Main procedure
def conquer(lst):
    # Base case. Nothing left to sort.
    if len(lst) == 1:
        return lst
    left, right = divide(lst)
    left = conquer(left)        # Sort the halves separately
    right = conquer(right)

    return merge(left, right)   # Combine them

# To merge two sorted arrays we need to take the smaller
# element of the tops of two arrays and put it in the third
# array, then repeat the process.
# When one of the arrays runs out we can just put the
# remaining array's elements into the third array.
def merge(left, right):
    combined = []
    while len(left) > 0 and len(right) > 0:
        if left[0] <= right[0]:
            combined.append(left.pop(0))
        else:
            combined.append(right.pop(0))
    # Consume remaining array, in case if left and right
    # were of different lengths.
    # Only one of the following loops will be entered.
    while len(left) > 0:
        combined.append(left.pop(0))
    while len(right) > 0:
        combined.append(right.pop(0))
    return combined

print(conquer(random_list))

