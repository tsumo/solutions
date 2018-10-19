#!/usr/bin/env python3

"""
Binary search tree from scratch
"""

import random
random.seed()

class BinSearchTree:
    def __init__(self, val):
        self.root = val
        self.l = self.r = None

    def insert(self, val):
        if self.root >= val:
            if self.l == None:
                self.l = BinSearchTree(val)
            else:
                self.l.insert(val)
        else:
            if self.r == None:
                self.r = BinSearchTree(val)
            else:
                self.r.insert(val)

    def search(self, val, steps = 1):
        if self.root == val:
            return steps
        if self.root >= val:
            if self.l != None:
                return self.l.search(val, steps + 1)
            else:
                return None
        else:
            if self.r != None:
                return self.r.search(val, steps + 1)
            else:
                return None

    def print(self, level = 0):
        print(' ' * level, self.root)

        if isinstance(self.l, BinSearchTree):
            self.l.print(level + 1)

        if isinstance(self.r, BinSearchTree):
            self.r.print(level + 1)

l = [x for x in range(10)]
random.shuffle(l)

t = BinSearchTree(l.pop())
for i in l:
    t.insert(i)

t.print()

s = random.randrange(10)
print('Found', s, 'in', t.search(s), 'steps')

