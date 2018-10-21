#!/usr/bin/env python3

"""
Binary heap (priority queue) from scratch.

Heap is a tree where value of the parent is always more or equal (max heap) /
less or equal (min heap) compared to the value of its children.

Complete binary tree is one where each level has all of its nodes, except the
bottom layer which fills from left to the right. Such tree can be stored in
memory as the continiuos array, where node at position n has its children at
positions 2n+1 and 2n+2 for 0-based array and 2n and 2n+1 for 1-based array.
"""

import math
import random

class BinHeap:
    def __init__(self, max_heap = True):
        # Single wasted value at the start of the list
        # helps to simplify index math
        self.heap = [0]
        self.size = 0
        # Max heap by default
        self.max_heap = max_heap

    def insert(self, k):
        # Adding new element to the end of the list preserves
        # properties of complete binary tree
        self.heap.append(k)
        self.size += 1
        # Newly added element may need to be percolated up the
        # tree structure to preserve heap property
        self.percUp(self.size)

    def percUp(self, i):
        # While not at the top level
        while i // 2 > 0:
            # Swap if elements are out of order.
            # Max heap and min heap differ only in comparison operator
            if self.max_heap:
                if self.heap[i] > self.heap[i // 2]:
                    self.heap[i], self.heap[i // 2] = self.heap[i // 2], self.heap[i]
            else:
                if self.heap[i] < self.heap[i // 2]:
                    self.heap[i], self.heap[i // 2] = self.heap[i // 2], self.heap[i]
            i = i // 2

    def remove(self):
        # Retrieve value at the root of the tree
        k = self.heap[1]
        # Move the last value into the first position
        self.heap[1] = self.heap[self.size]
        self.size -= 1
        # Remove last value
        self.heap.pop()
        # New root element probably needs to be percolated down the
        # tree structure to preserve heap property
        self.percDown(1)
        return k

    def percDown(self, i):
        # While not at the bottom level
        while i * 2 <= self.size:
            # Select the child to swap places with
            child = self.min_max_child(i)
            # Swap in elements are out of order
            if self.max_heap:
                if self.heap[i] < self.heap[child]:
                    self.heap[i], self.heap[child] = self.heap[child], self.heap[i]
            else:
                if self.heap[i] > self.heap[child]:
                    self.heap[i], self.heap[child] = self.heap[child], self.heap[i]
            i = child

    def min_max_child(self, i):
        # Selects min or max child of node i
        left = i * 2
        right = i * 2 + 1
        # If node has only one child
        if right > self.size:
            return left

        if self.max_heap:
            if self.heap[left] > self.heap[right]:
                return left
            else:
                return right
        else:
            if self.heap[left] < self.heap[right]:
                return left
            else:
                return right

    def print(self):
        print('heap:', self.heap)
        print('size:', self.size)
        if self.size > 0:
            height = int(math.log(self.size, 2) + 1)
        else:
            height = 0
        print('height:', height)

        # List of lists to hold values from each level of tree
        structure = []
        for _ in range(height):
            structure.append([])

        # Add root element
        if self.size >= 1:
            structure[0].append(self.heap[1])

        i = 1

        # Add all other elements
        while i * 2 <= self.size:
            # Level of the node i
            level = int(math.log(i, 2) + 1)
            # Children of the node i
            n = 2 * i
            m = 2 * i + 1
            # Add children at their appropriate level, if they exist
            if n <= self.size:
                structure[level].append(self.heap[n])
            if m <= self.size:
                structure[level].append(self.heap[m])
            i += 1

        print('structure:')
        for row in structure:
            for elem in row:
                print(elem, end=' ')
            print()

# Creates max heap
h = BinHeap()

# Creates min heap
# h = BinHeap(False)

for _ in range(15):
    h.insert(random.randrange(100))

h.print()
h.remove()
print()
h.print()
# for _ in range(15):
#     h.remove()
#     h.print()

