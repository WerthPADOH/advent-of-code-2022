"""https://adventofcode.com/2022/day/24"""
from dataclasses import dataclass
from typing import List, Set, Tuple
import primes


class Valley:
    def __init__(self, lines) -> None:
        lines = [line.strip() for line in lines]
        self.width = len(lines[0]) - 2
        self.height = len(lines) - 2
        self._all_ground = {
            (x, y)
            for x in range(self.width)
            for y in range(self.height)
        }
        self.start = (0, -1)
        self.finish = (self.width - 1, self.height)
        self._all_ground.add(self.start)
        self._all_ground.add(self.finish)
        reset_round = primes.lcm(self.width, self.height)
        self._open_at_round = []
        for _ in range(reset_round):
            self._open_at_round.append(self._all_ground.copy())
        for y, row in enumerate(lines[1:-1]):
            row = row[1:-1]
            for x, char in enumerate(row):
                if char == '^':
                    for rd in range(reset_round):
                        location = (x, (y - rd) % self.height)
                        self._open_at_round[rd].discard(location)
                elif char == '>':
                    for rd in range(reset_round):
                        location = ((x + rd) % self.width, y)
                        self._open_at_round[rd].discard(location)
                elif char == 'v':
                    for rd in range(reset_round):
                        location = (x, (y + rd) % self.height)
                        self._open_at_round[rd].discard(location)
                elif char == '<':
                    for rd in range(reset_round):
                        location = ((x - rd) % self.width, y)
                        self._open_at_round[rd].discard(location)

    def open_at_round(self, n):
        return self._open_at_round[n % len(self._open_at_round)]

    def reachable(self, coords: Tuple[int]) -> Set[Tuple[int]]:
        neighbors = {
            coords,
            (coords[0], coords[1] + 1),
            (coords[0], coords[1] - 1),
            (coords[0] + 1, coords[1]),
            (coords[0] - 1, coords[1]),
        }
        return neighbors & self._all_ground


def fastest_path(valley: Valley) -> List[Tuple[int]]:
    """
    >>> lines = [
    ...     '#.#####',
    ...     '#.....#',
    ...     '#>....#',
    ...     '#.....#',
    ...     '#...v.#',
    ...     '#.....#',
    ...     '#####.#',
    ... ]
    >>> v = Valley(lines)
    >>> fp = fastest_path(v)
    >>> len(fp) - 1
    10
    >>> lines = [
    ...     '#.######',
    ...     '#>>.<^<#',
    ...     '#.<..<<#',
    ...     '#>v.><>#',
    ...     '#<^v^^>#',
    ...     '######.#',
    ... ]
    >>> v = Valley(lines)
    >>> fp = fastest_path(v)
    >>> len(fp) - 1
    18
    """
    paths = [(valley.start, )]
    next_paths = []
    winner = None
    round = 1
    open_spaces = valley.open_at_round(round)
    while winner is None:
        if not paths:
            if not next_paths:
                raise RuntimeError('All paths are dead ends!')
            round += 1
            paths = next_paths
            next_paths = []
            open_spaces = valley.open_at_round(round)
            continue
        pp = paths.pop()
        if pp[-1] == valley.finish:
            winner = pp
            break
        possible = valley.reachable(pp[-1]) & open_spaces
        for choice in possible:
            extended = pp + (choice, )
            next_paths.append(extended)
    return winner


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day24.txt') as infile:
        valley = Valley(infile)

    # Part 1
    path1 = fastest_path(valley)
    print(len(path1) - 1)
