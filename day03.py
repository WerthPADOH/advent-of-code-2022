"""https://adventofcode.com/2022/day/3"""

from functools import reduce


def find_repeats(sacks):
    repeated = []
    for sack in sacks:
        halfway = len(sack) // 2
        compartment1 = set(sack[:halfway])
        compartment2 = set(sack[halfway:])
        repeated.extend(compartment1 & compartment2)
    return repeated


def total_priority(chars):
    ordering = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    return sum(ordering.index(char) + 1 for char in chars)


def sum_repeat_priority(sacks):
    """
    >>> rs = [
    ...     'vJrwpWtwJgWrhcsFMMfFFhFp',
    ...     'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL',
    ...     'PmmdzqPrVvPwwTWBwg',
    ...     'wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn',
    ...     'ttgJtRGJQctTZtZT',
    ...     'CrZsJsPPZsGzwwsLwLmpwMDw',
    ... ]
    >>> sum_repeat_priority(rs)
    157
    """
    repeats = find_repeats(sacks)
    return total_priority(repeats)


def sum_trio_badges(sacks):
    """
    >>> rs = [
    ...     'vJrwpWtwJgWrhcsFMMfFFhFp',
    ...     'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL',
    ...     'PmmdzqPrVvPwwTWBwg',
    ...     'wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn',
    ...     'ttgJtRGJQctTZtZT',
    ...     'CrZsJsPPZsGzwwsLwLmpwMDw',
    ... ]
    >>> sum_trio_badges(rs)
    70
    """
    sacks = list(sacks)
    badges = []
    while sacks:
        trio = [set(ss) for ss in sacks[-3:]]
        del sacks[-3:]
        common = reduce(lambda x, y: x & y, trio)
        assert len(common) == 1
        badges.append(common.pop())
    return total_priority(badges)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    with open('day03.txt') as infile:
        rucksacks = [line.strip() for line in infile.readlines()]
    # Part 1
    print(sum_repeat_priority(rucksacks))
    # Part 2
    print(sum_trio_badges(rucksacks))
