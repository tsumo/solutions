#!/usr/bin/env python3

"""
Fibonacci numbers with memoization
"""

nums = {1: 1, 2: 1}

def fib(n):
    global nums

    if n in nums:               # If value was previously calculated
        return nums[n]

    num = fib(n-1) + fib(n-2)   # Find recursively

    nums[n] = num               # Memoize new value

    return num

print(fib(1000))

