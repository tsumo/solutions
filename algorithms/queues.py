#!/usr/bin/env python3

"""
Queue from scratch
"""

import math

class Node:
    def __init__(self, val):
        self.val = val
        self.prev = None

class Queue:
    def __init__(self):
        self.top = None
        self.bottom = None

        self.length = 0

    def enq(self, val):
        new = Node(val)
        if self.length == 0:
            self.top = self.bottom = new
        else:
            self.top.prev = new
            self.top = new

        self.length += 1

    def deq(self):
        if self.length == 0:
            return None
        val = self.bottom.val
        self.bottom = self.bottom.prev

        self.length -= 1

        return val

    def print_all(self):
        curr = self.bottom
        while curr != None:
            print(curr.val)
            curr = curr.prev

q = Queue()
q.enq(3)
q.enq(6)
q.enq(9)
q.enq(12)
q.enq(32)
print(q.deq())
print(q.deq())
print(q.deq())
print(q.deq())
print(q.deq())

