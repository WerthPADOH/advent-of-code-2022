"""https://adventofcode.com/2022/day/20"""
from collections import deque


def mix_numbers(numbers, times=1, multiplier=1, steps=-1):
    """
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=1)
    [2, 1, -3, 3, -2, 0, 4]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=2)
    [1, -3, 2, 3, -2, 0, 4]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=3)
    [1, 2, 3, -2, -3, 0, 4]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=4)
    [1, 2, -2, -3, 0, 3, 4]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=5)
    [1, 2, -3, 0, 3, 4, -2]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=6)
    [1, 2, -3, 0, 3, 4, -2]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], steps=7)
    [1, 2, -3, 4, 0, 3, -2]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4])
    [1, 2, -3, 4, 0, 3, -2]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=1, multiplier=811589153)
    [0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=2, multiplier=811589153)
    [0, 2434767459, 1623178306, 3246356612, -2434767459, -1623178306, 811589153]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=3, multiplier=811589153)
    [0, 811589153, 2434767459, 3246356612, 1623178306, -1623178306, -2434767459]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=4, multiplier=811589153)
    [0, 1623178306, -2434767459, 811589153, 2434767459, 3246356612, -1623178306]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=5, multiplier=811589153)
    [0, 811589153, -1623178306, 1623178306, -2434767459, 3246356612, 2434767459]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=6, multiplier=811589153)
    [0, 811589153, -1623178306, 3246356612, -2434767459, 1623178306, 2434767459]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=7, multiplier=811589153)
    [0, -2434767459, 2434767459, 1623178306, -1623178306, 811589153, 3246356612]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=8, multiplier=811589153)
    [0, 1623178306, 3246356612, 811589153, -2434767459, 2434767459, -1623178306]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=9, multiplier=811589153)
    [0, 811589153, 1623178306, -2434767459, 3246356612, 2434767459, -1623178306]
    >>> mix_numbers([1, 2, -3, 3, -2, 0, 4], times=10, multiplier=811589153)
    [0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153]
    """
    numbers = list(numbers)
    length = len(numbers)
    shifts = [nn % (length - 1) for nn in numbers]
    indices = list(range(len(numbers)))
    if multiplier != 1:
        numbers = [nn * multiplier for nn in numbers]
    for _ in range(times):
        for ii, shift in enumerate(shifts):
            if steps == ii:
                break
            index = indices[ii]
            new_index = (index + shift) % (length - 1)
            if new_index == index:
                continue
            elif new_index == 0:
                new_index = length - 1
            if new_index < index:
                passing = set(range(new_index, index))
                for jj in range(len(indices)):
                    if indices[jj] in passing:
                        indices[jj] += 1
                indices[ii] = new_index
            if new_index >= index:
                passing = range(index + 1, new_index + 1)
                for jj in range(len(indices)):
                    if indices[jj] in passing:
                        indices[jj] -= 1
                indices[ii] = new_index
    mixed = [None] * length
    for kk, index in enumerate(indices):
        mixed[index] = numbers[kk]
    return mixed


def grove_coordinates(numbers: list):
    """
    >>> grove_coordinates([1, 2, -3, 4, 0, 3, -2])
    [4, -3, 2]
    """
    zero_pos = numbers.index(0)
    return [
        numbers[(zero_pos + ii) % len(numbers)]
        for ii in (1000, 2000, 3000)
    ]


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    with open('day20.txt') as infile:
        in_numbers = tuple(int(line.strip()) for line in infile)

    # Part 1
    mixed = mix_numbers(in_numbers)
    print(sum(grove_coordinates(mixed)))
