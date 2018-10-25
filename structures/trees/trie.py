#!/usr/bin/env python3

"""
Trie from scratch
"""

class Node:
    def __init__(self, val):
        self.val = val
        self.children = []
        self.term = False

    def print(self, level = 0):
        print(' '*level, self.val, '!' if self.term else '')
        for child in self.children:
            child.print(level + 1)

class Trie:
    def __init__(self):
        # Dummy node
        self.root = Node(None)

    def insert(self, val):
        curr = self.root
        while len(val) > 0:
            flag = False
            char = val[0]
            val = val[1:]
            for node in curr.children:
                if node.val == char:
                    curr = node
                    flag = True
                    break
            if flag == False:
                curr.children.append(Node(char))
                curr = curr.children[-1]
        curr.term = True

    def print(self):
        self.root.print()

t = Trie()
t.insert('cat')
t.insert('catch')
t.insert('car')
t.insert('bat')
t.insert('batch')
t.insert('carburator')
t.print()

