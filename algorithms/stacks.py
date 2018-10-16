#!/usr/bin/env python3

"""
Stack from scratch
"""

import math

class Node:
    def __init__(self):
        self.val = None
        self.next = None

class Stack:
    def __init__(self):
        self.top = None

        self.length = 0

        self.min = []
        self.min.append(math.inf)

    def push(self, val):
        new = Node()
        new.val = val
        new.next = self.top
        self.top = new

        self.length += 1

        if self.min[-1] > val:
            self.min.append(val)

    def peek(self):
        return self.top.val

    def pop(self):
        val = self.top.val
        self.top = self.top.next

        self.length -= 1

        if val == self.min[-1]:
            self.min.pop()

        return val

    def is_empty(self):
        return self.length == 0

    def minimum(self):
        return self.min[-1]

    def print_all(self):
        curr = self.top
        acc = []
        while curr:
            acc.append(curr.val)
            curr = curr.next
        print('In/Out <->', acc)

    def sort(self):
        t = Stack()
        while not self.is_empty():
            elem = self.pop()
            while not t.is_empty() and t.peek() > elem:
                self.push(t.pop())
            t.push(elem)
        while not t.is_empty():
            self.push(t.pop())

print('  Normal stack operations')
s = Stack()
s.push(1)
s.push(2)
s.push(3)
s.print_all()
print(s.pop())
print(s.pop())
print(s.pop())

print('  Stack sorting')
s = Stack()
s.push(87)
s.push(34)
s.push(12)
s.push(46)
s.sort()
s.print_all()

class StackBasedQueue:
    def __init__(self):
        self.input = Stack()
        self.output = Stack()

    def push(self, val):
        self.input.push(val)

    def pop(self):
        while not self.input.is_empty():
            self.output.push(self.input.pop())
        return self.output.pop()

print('  Stack based queue')
sbq = StackBasedQueue()
sbq.push(1)
sbq.push(2)
sbq.push(3)
print(sbq.pop())
print(sbq.pop())
print(sbq.pop())

