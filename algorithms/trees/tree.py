#!/usr/bin/env python3

"""
Tree from scratch
"""

class Tree:
    def __init__(self):
        self.root = None
        self.nodes = []

    def depth_first(self):
        """ In pre-order """
        print(self.root)

        for node in self.nodes:
            if isinstance(node, Tree):
                node.depth_first()
            else:
                print(node)

    def breadth_first(self, root = True):
        if root:
            print(self.root)

        for node in self.nodes:
            if isinstance(node, Tree):
                print(node.root)
            else:
                print(node)

        for node in self.nodes:
            if isinstance(node, Tree):
                node.breadth_first(False)

def construct(lst):
    """
    Creates a tree from list.
    First element is root value, others are children nodes.
    (values or subtrees).
    """
    t = Tree()
    t.root = lst[0]
    for node in lst[1:]:
        if isinstance(node, list):
            t.nodes.append(construct(node))
        else:
            t.nodes.append(node)
    return t

#      0
#    / | \
#   1  2  3
#  / \    |
# 4   5   6
t = construct([ 0, [ 1, 4, 5 ], 2, [ 3, 6 ] ])

t.depth_first()                 # 0 1 4 5 2 3 6
print()
t.breadth_first()               # 0 1 2 3 4 5 6
print()

