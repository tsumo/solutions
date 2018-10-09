#!/usr/bin/env python3

import random

random.seed();

def random_list(length, max_range):
    lst = []
    for i in range(length):
        lst.append(random.randrange(max_range))
    return lst

