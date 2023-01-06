"""https://adventofcode.com/2022/day/14"""

from typing import Dict, Tuple, Set


def parse_rock_path(text: str) -> Set[Tuple]:
    """
    >>> parse_rock_path('498,4 -> 498,6 -> 496,6')
    {(496, 6), (497, 6), (498, 5), (498, 4), (498, 6)}
    >>> long = parse_rock_path('503,4 -> 502,4 -> 502,9 -> 494,9')
    >>> expected = {
    ...     (503, 4), (502, 4),
    ...     (502, 5), (502, 6), (502, 7), (502, 8), (502, 9),
    ...     (501, 9), (500, 9), (499, 9), (498, 9), (497, 9), (496, 9), (495, 9), (494, 9)
    ... }
    >>> long == expected
    True
    """
    joints_text = text.strip().split(' -> ')
    joint_coords = []
    for jt in joints_text:
        x, y = jt.split(',')
        joint_coords.append((int(x), int(y)))
    if len(joint_coords) == 1:
        return joint_coords
    path = set(joint_coords)
    for ii in range(len(joint_coords) - 1):
        x1, y1 = joint_coords[ii]
        x2, y2 = joint_coords[ii + 1]
        if (x1, y1) == (x2, y2):
            continue
        elif x1 == x2:
            if y1 > y2:
                y1, y2 = y2, y1
            path.update((x1, yy) for yy in range(y1 + 1, y2))
        elif y1 == y2:
            if x1 > x2:
                x1, x2 = x2, x1
            path.update((xx, y1) for xx in range(x1 + 1, x2))
    return path


ROCK = 0
SAND = 1


class Granule:
    def __init__(self, coords: Tuple) -> None:
        self.coords = list(coords)
    
    def move_to(self, coords: Tuple) -> None:
        self.coords = list(coords)

    def below(self) -> Tuple[int]:
        return (self.coords[0], self.coords[1] + 1)
    
    def below_left(self) -> Tuple[int]:
        return (self.coords[0] - 1, self.coords[1] + 1)
    
    def below_right(self) -> Tuple[int]:
        return (self.coords[0] + 1, self.coords[1] + 1)


def flow_sand(
    rock_coords: Set[Tuple],
    add_floor=False,
    origin: Tuple=(500, 0),
) -> Dict[Tuple, str]:
    """
    >>> rocks = parse_rock_path('498,4 -> 498,6 -> 496,6')
    >>> rocks.update(parse_rock_path('503,4 -> 502,4 -> 502,9 -> 494,9'))
    >>> res = flow_sand(rocks)
    >>> sum(val == SAND for val in res.values())
    24
    >>> res2 = flow_sand(rocks, add_floor=True)
    >>> sum(val == SAND for val in res2.values())
    93
    """
    origin = tuple(origin)
    contents = dict.fromkeys(rock_coords, ROCK)
    bottom = max(y for x, y in rock_coords)
    # If there's an infinite floor below, it just needs to be wide enough
    # to hold a full pyramid up to the sand origin.
    if add_floor:
        floor_y = bottom + 2
        for floor_x in range(origin[0] - floor_y, origin[0] + floor_y + 1):
            contents[(floor_x, floor_y)] = ROCK
        bottom = floor_y
    still_piling = True
    while still_piling:
        grain = origin
        still_falling = True
        while still_falling:
            # Stop if falling off the map
            if grain[1] >= bottom:
                still_piling = False
                break
            below = (grain[0], grain[1] + 1)
            if below not in contents:
                grain = below
                continue
            bottom_left = (grain[0] - 1, grain[1] + 1)
            if bottom_left not in contents:
                grain = bottom_left
                continue
            bottom_right = (grain[0] + 1, grain[1] + 1)
            if bottom_right not in contents:
                grain = bottom_right
                continue
            # If cannot fall anymore, stops where it is
            contents[grain] = SAND
            still_falling = False
        # Stop if the origin is blocked
        if grain == origin:
            still_piling = False
    return contents


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day14.txt') as infile:
        rocks = set()
        for line in infile:
            rocks.update(parse_rock_path(line))

    # Part 1
    solid_coords = flow_sand(rocks)
    print(sum(square == SAND for square in solid_coords.values()))

    # Part 2
    with_floor = flow_sand(rocks, add_floor=True)
    print(sum(square == SAND for square in with_floor.values()))
    