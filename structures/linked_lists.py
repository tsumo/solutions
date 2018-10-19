#!/usr/bin/env python3

class sll:
    """Singly linked list"""

    def __init__(self, *args):
        if len(args) == 0:
            self.val = self.next = None
        else:
            self.val = args[0]
            self.next = sll(*args[1:])

    def print(self):
        if self.val != None:
            print(self.val)
        if self.next:
            self.next.print()

    def map(self, func):
        if self.val:
            self.val = func(self.val)
        if self.next:
            self.next.map(func)

    def delete_n(self, n):
        if not self.next or n < 0:
            return
        if n == 0:
            self.val = self.next.val
            self.next = self.next.next
        else:
            self.next.delete_n(n-1)

    def insert(self, val):
        new = sll(val)
        new.next = self
        self = new
        return self


nodes = sll(1, 2, 3, 4, 5)
nodes = nodes.insert(9)
nodes.map(lambda x: 2**x)
nodes.delete_n(1)
# nodes.print()

"""
Write a summation function for linked lists
513 + 294 = 807
(3 -> 1 -> 5) + (4 -> 9 -> 2) = (7 -> 0 -> 8)
"""

def sum_ll(l1, l2):
    res = sll()
    carry = False
    while l1.val or l2.val or carry:
        if carry:
            s = 1
            carry = False
        else:
            s = 0
        if l1.val:
            s += l1.val
        if l2.val:
            s += l2.val
        if s >= 10:
            carry = True
            s -= 10
        res = res.insert(s)
        l1 = l1.next
        l2 = l2.next
    return res

l1 = sll(3, 1, 5)
l2 = sll(4, 9, 2)
summed = sum_ll(l1, l2)
# summed.print()

def detect_loop(l):
    """
    Find beginning of a loop in a circular linked list.
    1 -> 2 -> 3 -> 4
         ^         v
         7 <- 6 <- 5
    """
    tort = l.next
    hare = l.next.next
    while tort != hare:
        tort = tort.next
        hare = hare.next.next
    i = 0
    tort = l
    while tort != hare:
        tort = tort.next
        hare = hare.next
        i += 1
    print(i)

loop = sll(1, 2, 3, 4, 5, 6, 7, 8, 9)
loop.next.next.next.next.next = loop.next.next.next
detect_loop(loop)

