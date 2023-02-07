"""https://adventofcode.com/2022/day/18"""
from collections import defaultdict


def parse_cubes(stream):
    cubes = []
    for line in stream:
        parts = line.strip().split(',')
        cubes.append(tuple(int(pp) for pp in parts))
    return cubes


def adjacent_cubes(cube: tuple) -> set:
    return {
        (cube[0] - 1, cube[1], cube[2]),
        (cube[0] + 1, cube[1], cube[2]),
        (cube[0], cube[1] - 1, cube[2]),
        (cube[0], cube[1] + 1, cube[2]),
        (cube[0], cube[1], cube[2] - 1),
        (cube[0], cube[1], cube[2] + 1),
    }


def exposed_sides(cubes) -> int:
    """
    >>> cc = (
    ...     (2,2,2), (1,2,2), (3,2,2), (2,1,2), (2,3,2), (2,2,1), (2,2,3),
    ...     (2,2,4), (2,2,6), (1,2,5), (3,2,5), (2,1,5), (2,3,5),
    ... )
    >>> exposed_sides(cc)
    64
    """
    visible = 6 * len(cubes)
    seen = set()
    for cube in cubes:
        for adj in adjacent_cubes(cube):
            if adj in seen:
                visible -= 2
        seen.add(cube)
    return visible


def exterior_sides(cubes) -> int:
    """
    >>> cc = (
    ...     (2,2,2), (1,2,2), (3,2,2), (2,1,2), (2,3,2), (2,2,1), (2,2,3),
    ...     (2,2,4), (2,2,6), (1,2,5), (3,2,5), (2,1,5), (2,3,5),
    ... )
    >>> exterior_sides(cc)
    58
    """
    # Going to use a "viral" check for external air. Start with one air cube
    # known to be outside, then go through its neighbors. If the neighbor is
    # solid, count that side. If the neighbor's air, add it to be checked the
    # same way.
    cubes = set(cubes)
    bounds = [[None, None], [None, None], [None, None]]
    for cube in cubes:
        for ii in range(3):
            if bounds[ii][0] is None or cube[ii] < bounds[ii][0]:
                bounds[ii][0] = cube[ii]
            if bounds[ii][1] is None or cube[ii] > bounds[ii][1]:
                bounds[ii][1] = cube[ii]
    bounds = [[low - 1, high + 1] for low, high in bounds]
    corner_air = (bounds[0][0], bounds[1][0], bounds[2][0])
    exterior_air = {corner_air}
    checked = set()
    sides = 0
    while exterior_air:
        air = exterior_air.pop()
        checked.add(air)
        for adj in adjacent_cubes(air):
            if adj in checked:
                continue
            if adj in cubes:
                sides += 1
                continue
            in_bounds = (
                low <= air_coord <= high
                for air_coord, (low, high) in zip(air, bounds)
            )
            if all(in_bounds):
                exterior_air.add(adj)
    return sides


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day18.txt') as infile:
        cubes = parse_cubes(infile)

    # Part 1
    print(exposed_sides(cubes))

    # Part 2
    print(exterior_sides(cubes))
