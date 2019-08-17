#!/usr/bin/env python3

"""
Problem 19

You are given the following information, but you may prefer to do some research
for yourself.

- 1 Jan 1900 was a Monday.
- Thirty days has September,
  April, June and November.
  All the rest have thirty-one,
  Saving February alone,
  Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
- A leap year occurs on any year evenly divisible by 4, but not on a century
  unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century
(1 Jan 1901 to 31 Dec 2000)?
"""


import pe_utils


def problem19():
    months = {
        0: 31,  # January
        1: 28,  # February
        2: 31,  # March
        3: 30,  # April
        4: 31,  # May
        5: 30,  # June
        6: 31,  # July
        7: 31,  # August
        8: 30,  # September
        9: 31,  # October
        10: 30, # November
        11: 31, # December
    }
    start_year = 1900
    end_year = 2000

    n_days = 0
    result = 0
    for year in range(start_year, end_year + 1):
        leap_year = (year % 1000 == 0 and year % 400 == 0) or (year % 1000 != 0 and year % 4 == 0)
        for month in range(12):
            days = months[month]
            if month == 1 and leap_year:
                days = 29
            for day in range(days):
                n_days += 1
                if day == 0 and n_days % 7 == 0 and year > 1900:
                    result += 1
    return result


pe_utils.test(problem19, [], 171)

