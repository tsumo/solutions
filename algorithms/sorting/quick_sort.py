#!/usr/bin/env python3

"""
Memory: O(1)
    Can sort array in place

Time: O(n^2), generally O(n log n)

Divide and conquer algorithm.
First it chooses an arbitrary point in given array (last element here)
and divides all other elements in two piles - smaller than chosen
element and bigger than chosen element. Then it sorts those halves and
combines all of them.
"""

import sorting_utils

random_list = sorting_utils.random_list(50, 1000000)

def conquer(A):
    if len(A) == 0:                 # Base case, nothing to sort
        return []
                                    # Split array in two halves:
    left, pivot, right = divide(A)  # elements smaller than pivot
                                    # and elements bigger than pivot.
    left = conquer(left)            # Do the same to both halves,
    right = conquer(right)

    return [*left, pivot, *right]

def divide(A):
    pivot = A[-1]               # Choose last element as pivot
    left = []                   # Array for numbers smaller and
    right = []                  # bigger than pivot.
    for x in A[:-1]:            # Go through whole array,
        if x <= pivot:          # put elements in either left or right
            left.append(x)      # array.
        else:
            right.append(x)
    return left, pivot, right

random_list = conquer(random_list)
print(random_list)
assert random_list == sorted(random_list)

