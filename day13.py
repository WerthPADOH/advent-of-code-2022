"""https://adventofcode.com/2022/day/13"""
from collections import deque
from typing import Iterable, Generator


def parse_packets(stream) -> Generator:
    packets = []
    for line in stream:
        line = line.replace('\n', '')
        if line:
            packets.append(eval(line))
            if len(packets) == 2:
                yield packets
                packets = []


# Unsafe for general code, but meh.  Let's hurry onto the next day.
def correct_order(packet1: Iterable, packet2: Iterable) -> bool:
    """
    >>> correct_order([1,1,3,1,1], [1,1,5,1,1])
    True
    >>> correct_order([[1],[2,3,4]], [[1],4])
    True
    >>> correct_order([9], [[8,7,6]])
    False
    >>> correct_order([[4,4],4,4], [[4,4],4,4,4])
    True
    >>> correct_order([7,7,7,7], [7,7,7])
    False
    >>> correct_order([], [3])
    True
    >>> correct_order([[[]]], [[]])
    False
    >>> correct_order([1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9])
    False
    """
    packet1, packet2 = deque(packet1), deque(packet2)
    while packet1 and packet2:
        left, right = packet1.popleft(), packet2.popleft()
        if isinstance(left, int) and isinstance(right, int):
            if left < right:
                return True
            elif left > right:
                return False
        else:
            left = [left] if isinstance(left, int) else left
            right = [right] if isinstance(right, int) else right
            res = correct_order(left, right)
            if res is not None:
                return res
    # Left lists should run out of items first
    if not packet1 and packet2:
        return True
    elif packet1 and not packet2:
        return False
    elif not packet1 and not packet2:
        return None
    raise RuntimeError('Finished logic without resolution')


def sort_by_bool_op(iterable, operator):
    """
    >>> packets = [
    ...     [1, 1, 3, 1, 1],
    ...     [1,1,5,1,1],
    ...     [[1],[2,3,4]],
    ...     [[1],4],
    ...     [9],
    ...     [[8,7,6]],
    ...     [[4,4],4,4],
    ...     [[4,4],4,4,4],
    ...     [7,7,7,7],
    ...     [7,7,7],
    ...     [],
    ...     [3],
    ...     [[[]]],
    ...     [[]],
    ...     [1,[2,[3,[4,[5,6,7]]]],8,9],
    ...     [1,[2,[3,[4,[5,6,0]]]],8,9],
    ... ]
    >>> packets.extend(( [[2]], [[6]] ))
    >>> sorted_packets = sort_by_bool_op(packets, correct_order)
    >>> for p in sorted_packets:
    ...     print(p)
    []
    [[]]
    [[[]]]
    [1, 1, 3, 1, 1]
    [1, 1, 5, 1, 1]
    [[1], [2, 3, 4]]
    [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]
    [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
    [[1], 4]
    [[2]]
    [3]
    [[4, 4], 4, 4]
    [[4, 4], 4, 4, 4]
    [[6]]
    [7, 7, 7]
    [7, 7, 7, 7]
    [[8, 7, 6]]
    [9]
    """
    iterable = list(iterable)
    pivot = iterable[0]
    lesser, greater = [], []
    for value in iterable[1:]:
        if operator(value, pivot):
            lesser.append(value)
        else:
            greater.append(value)
    if len(lesser) > 1:
        lesser = sort_by_bool_op(lesser, operator)
    if len(greater) > 1:
        greater = sort_by_bool_op(greater, operator)
    return lesser + [pivot] + greater


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day13.txt') as infile:
        lines = [line.replace('\n', '') for line in infile.readlines()]

    # Part 1
    total = 0
    for ii, (p1, p2) in enumerate(parse_packets(lines), 1):
        if correct_order(p1, p2):
            total += ii
    print(total)

    # Part 2
    all_packets = [[[2]], [[6]]]
    for pair in parse_packets(lines):
        all_packets.extend(pair)
    all_packets = sort_by_bool_op(all_packets, correct_order)
    product = 1
    for jj, packet in enumerate(all_packets, 1):
        if packet == [[2]] or packet == [[6]]:
            product *= jj
    print(product)
