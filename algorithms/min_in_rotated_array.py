#!/usr/bin/env python3

"""
Find a minimum element in a rotated array.
Rotated array example:
    from 1 2 3 4 5 6 7 8 9
    to   5 6 7 8 9 1 2 3 4
"""

arr = list(range(87, 1000))     # Easy array rotation
rot = arr[576:] + arr[:576]     # using slices.

def find_min(A):
    l = len(A)
    if l == 1:                  # Base case, only one element.
        return A[0]
    if A[0] < A[-1]:            # When first element is smaller than
        return A[0]             # the last it means array is not rotated.
    mid = l // 2                # Otherwise do binary search
    return min(find_min(A[:mid]), find_min(A[mid:]))

print(find_min(rot))

