"""https://adventofcode.com/2022/day/24"""
from dataclasses import dataclass
from typing import Dict, List, Set, Tuple
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
        self.reachable: Dict[Tuple[int], Set[Tuple[int]]] = dict()
        for coord in self._all_ground:
            x, y = coord
            neighbors = {coord, (x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)}
            self.reachable[coord] = neighbors & self._all_ground
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




def fastest_path(
    valley: Valley,
    start_round: int=0,
    backwards=False,
) -> List[Tuple[int]]:
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
    >>> fastest_path(v)
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
    >>> fastest_path(v)
    18
    """
    start = valley.start
    finish = valley.finish
    if backwards:
        start, finish = finish, start
    locations = {start}
    next_locations = set()
    round = start_round + 1
    open_spaces = valley.open_at_round(round)
    while True:
        if not locations:
            if not next_locations:
                raise RuntimeError('All paths are dead ends!')
            round += 1
            locations.update(next_locations)
            next_locations.clear()
            open_spaces = valley.open_at_round(round)
            continue
        loc = locations.pop()
        if loc == finish:
            break
        possible = valley.reachable[loc] & open_spaces
        for choice in possible:
            next_locations.add(choice)
    return round - 1


def forgot_something(valley: Valley):
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
    >>> forgot_something(v)
    30
    >>> lines = [
    ...     '#.######',
    ...     '#>>.<^<#',
    ...     '#.<..<<#',
    ...     '#>v.><>#',
    ...     '#<^v^^>#',
    ...     '######.#',
    ... ]
    >>> v = Valley(lines)
    >>> forgot_something(v)
    54
    """
    first_time = fastest_path(valley, start_round=0, backwards=False)
    back_time = fastest_path(valley, start_round=first_time, backwards=True)
    second_time = fastest_path(valley, start_round=back_time, backwards=False)
    return second_time


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day24.txt') as infile:
        valley = Valley(infile)

    # Part 1
    print(fastest_path(valley))

    # Part 2
    print(forgot_something(valley))
