#!/usr/bin/env python3

"""
Given a matrix in which each row and each column is sorted,
write a function to find an element in it.
"""

m = [[ 1,  2,  3,  4,  5],
     [11, 12, 13, 14, 15],
     [21, 22, 23, 24, 25],
     [31, 32, 33, 34, 35],
     [41, 42, 43, 44, 45],
     [51, 52, 53, 54, 55],
     [61, 62, 63, 64, 75],
     [71, 72, 73, 74, 76]]

def mat_search(m, val):
    # Find dimensions of matrix
    width = len(m[0])
    height = len(m)

    # Find submatrix in the upper left corner where
    # val can be found
    for bottom_border in range(height-1, -1, -1):
        if m[bottom_border][0] <= val:
            break

    for right_border in range(width-1, -1, -1):
        if m[0][right_border] <= val:
            break

    # Do iterative binary search starting from bottom border row
    # and going up if val was not found
    for row in range(bottom_border, -1, -1):
        low = 0
        high = right_border
        while low <= high:
            mid = (low + high) // 2
            if m[row][mid] > val:
                high = mid - 1
            elif m[row][mid] < val:
                low = mid + 1
            else:
                return (row, mid)

    return None

print(mat_search(m, 75))

