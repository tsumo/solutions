#!/usr/bin/env python3

"""
Fibonacci numbers with memoization
"""

nums = {1: 1, 2: 1}

def fib(n):
    global nums

    if n in nums:
        return nums[n]

    num = nums[n-1] + nums[n-2]
    nums[n] = num
    return num

for i in range(1, 1000):
    print(fib(i))

