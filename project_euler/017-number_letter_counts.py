#!/usr/bin/env python3

"""
Problem 17

If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with
British usage.
"""


import pe_utils


def number_to_words(n):
    units = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']
    teens = ['ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen']
    tens = ['twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety']
    if n < 10:
        res = units[n]
    elif n < 20:
        res = teens[n - 10]
    elif n <= 99:
        res = tens[(n // 10) - 2]
        if n % 10 != 0:
            res += '-' + units[n % 10]
    elif n <= 999:
        res = units[n // 100] + ' hundred'
        if n % 100 != 0:
            res += ' and ' + number_to_words(n % 100)
    elif n == 1000:
        res = units[n // 1000] + ' thousand'
    else:
        res = None
    return res


test_cases = {
    0: 'zero',
    3: 'three',
    7: 'seven',
    10: 'ten',
    14: 'fourteen',
    20: 'twenty',
    21: 'twenty-one',
    56: 'fifty-six',
    99: 'ninety-nine',
    100: 'one hundred',
    209: 'two hundred and nine',
    300: 'three hundred',
    417: 'four hundred and seventeen',
    560: 'five hundred and sixty',
    698: 'six hundred and ninety-eight',
    1000: 'one thousand',
}


for n in test_cases:
    assert number_to_words(n) == test_cases[n]


def problem17(n):
    result = 0
    for i in range(1, n + 1):
        result += len(number_to_words(i).replace(' ', '').replace('-', ''))
    return result


pe_utils.test(problem17, [5], 19)
pe_utils.test(problem17, [1000], 21124)

