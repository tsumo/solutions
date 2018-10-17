#!/usr/bin/env python3

"""
Binary tree from scratch
"""

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

b.in_order()            # In-Order   3 1 5 4 6 0 2
print()
b.pre_order()           # Pre-Order  0 1 3 4 5 6 2
print()
b.post_order()          # Post-Order 3 5 6 4 1 2 0
print()
b.breadth_first()       # Breadth-First 0 1 2 3 4 5 6

