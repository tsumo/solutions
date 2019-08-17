#!/usr/bin/env python3

"""
Binary tree from scratch
"""

from math import inf

class BinTree:

    def __init__(self):
        self.root = self.l = self.r = None


    def in_order(self):
        """ Left, current, right """
        if isinstance(self.l, BinTree):
            self.l.in_order()
        else:
            print(self.l)

        print(self.root)

        if isinstance(self.r, BinTree):
            self.r.in_order()
        else:
            print(self.r)


    def pre_order(self):
        """ Current, left, right """
        print(self.root)

        if isinstance(self.l, BinTree):
            self.l.pre_order()
        else:
            print(self.l)

        if isinstance(self.r, BinTree):
            self.r.pre_order()
        else:
            print(self.r)


    def post_order(self):
        """ Left, right, current """
        if isinstance(self.l, BinTree):
            self.l.post_order()
        else:
            print(self.l)

        if isinstance(self.r, BinTree):
            self.r.post_order()
        else:
            print(self.r)

        print(self.root)


    def breadth_first(self, root = True):
        """ Level by level """
        if root:
            print(self.root)

        if isinstance(self.l, BinTree):
            print (self.l.root)
        else:
            print(self.l)

        if isinstance(self.r, BinTree):
            print (self.r.root)
        else:
            print(self.r)

        if isinstance(self.l, BinTree):
            self.l.breadth_first(False)

        if isinstance(self.r, BinTree):
            self.r.breadth_first(False)


    def print(self, root = True, level = 0):
        ind = ' ' * level

        if root:
            print(ind, self.root)

        if isinstance(self.l, BinTree):
            print(ind, self.l.root)
        else:
            print(ind, self.l)

        if isinstance(self.r, BinTree):
            print(ind, self.r.root)
        else:
            print(ind, self.r)

        if isinstance(self.l, BinTree):
            self.l.print(False, level + 1)

        if isinstance(self.r, BinTree):
            self.r.print(False, level + 1)


    def max_path(self):
        """
        Find a path which will have the largest sum.
        """
        left = self.l.max_path() if isinstance(self.l, BinTree) else self.l
        right = self.r.max_path() if isinstance(self.r, BinTree) else self.r
        return max(self.root + left, self.root + right)


    def max_uniq_path(self, visited = []):
        """
        Find a path that consists only of unique numbers
        which will have the largest sum.
        """
        if self.l in visited:
            left = -inf
        elif isinstance(self.l, BinTree):
            left = self.l.max_uniq_path(visited + [self.root])
        else:
            left = self.l

        if self.r in visited:
            right = -inf
        elif isinstance(self.r, BinTree):
            right = self.r.max_uniq_path(visited + [self.root])
        else:
            right = self.r
        return max(self.root + left, self.root + right)

b = BinTree()           #      0
b.root = 0              #     / \
b.l = BinTree()         #    1   2
b.l.root = 1            #   / \
b.l.l = 3               #  3   4
b.l.r = BinTree()       #     / \
b.l.r.root = 4          #    5   6
b.l.r.l = 5
b.l.r.r = 6
b.r = 2

print('Pretty printing')
b.print()
print('In-Order')
b.in_order()            # In-Order   3 1 5 4 6 0 2
print('Pre-Order')
b.pre_order()           # Pre-Order  0 1 3 4 5 6 2
print('Post-Order')
b.post_order()          # Post-Order 3 5 6 4 1 2 0
print('Breadth-First')
b.breadth_first()       # Breadth-First 0 1 2 3 4 5 6


b = BinTree()
b.root = 5
b.l = BinTree()
b.l.root = 3
b.l.l = BinTree()
b.l.l.root = 8
b.l.l.l = 2
b.l.l.r = 5
b.l.r = BinTree()
b.l.r.root = 4
b.l.r.l = 8
b.l.r.r = 1
b.r = BinTree()
b.r.root = 2
b.r.l = BinTree()
b.r.l.root = 7
b.r.l.l = 3
b.r.l.r = 5
b.r.r = BinTree()
b.r.r.root = 1
b.r.r.l = 6
b.r.r.r = 8
print()
print('Pretty printing')
b.print()
print('Max path')
print(b.max_path())
print('Max uniq path')
print(b.max_uniq_path())

