"""https://adventofcode.com/2022/day/17"""
from itertools import cycle


class RockShape:
    def __init__(self, coords, bottom_edge, left_edge) -> None:
        self.coords = []
        leftmost = None
        rightmost = None
        lowest = None
        highest = None
        self._left_coord = 0
        self._right_coord = 0
        self._low_coord = 0
        self._high_coord = 0
        for ii, pair in enumerate(coords):
            pair = tuple(pair)
            self.coords.append(pair)
            if leftmost is None or pair[0] < leftmost:
                self._left_coord = ii
                leftmost = pair[0]
            if rightmost is None or pair[0] > rightmost:
                self._right_coord = ii
                rightmost = pair[0]
            if lowest is None or pair[1] < lowest:
                self._low_coord = ii
                lowest = pair[1]
            if highest is None or pair[1] > highest:
                self._high_coord = ii
                highest = pair[1]
        self.move_down(lowest - bottom_edge)
        self.move_side(left_edge - leftmost)

    def move_side(self, n: int):
        """Move n units to the side. Negative for left, positive for right"""
        self.coords = [(x + n, y) for x, y in self.coords]

    def move_down(self, n: int):
        """Fall n units. Negative to go up, positive to go down."""
        self.coords = [(x, y - n) for x, y in self.coords]

    def left_edge(self) -> int:
        return self.coords[self._left_coord][0]

    def right_edge(self) -> int:
        return self.coords[self._right_coord][0]

    def bottom_edge(self) -> int:
        return self.coords[self._low_coord][1]

    def top_edge(self) -> int:
        return self.coords[self._high_coord][1]


class HorizontalLine(RockShape):
    def __init__(self, bottom_edge, left_edge):
        coords = [(0, 0), (1, 0), (2, 0), (3, 0)]
        super().__init__(coords, bottom_edge, left_edge)


class Cross(RockShape):
    def __init__(self, bottom_edge, left_edge) -> None:
        coords = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
        super().__init__(coords, bottom_edge, left_edge)


class Angle(RockShape):
    def __init__(self, bottom_edge, left_edge) -> None:
        coords = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
        super().__init__(coords, bottom_edge, left_edge)


class VerticalLine(RockShape):
    def __init__(self, bottom_edge, left_edge) -> None:
        coords = [(0, 0), (0, 1), (0, 2), (0, 3)]
        super().__init__(coords, bottom_edge, left_edge)


class Block(RockShape):
    def __init__(self, bottom_edge, left_edge) -> None:
        coords = [(0, 0), (0, 1), (1, 0), (1, 1)]
        super().__init__(coords, bottom_edge, left_edge)


def print_state(settled: set, cave_width: int, shape: RockShape):
    grid = [['.'] * cave_width for _ in range(shape.top_edge() + 1)]
    for x, y in settled:
        grid[y][x] = '#'
    for x, y in shape.coords:
        grid[y][x] = '@'
    lines = ('|' + ''.join(line) + '|' for line in reversed(grid))
    print('\n'.join(lines))
    print('+' + '-' * cave_width + '+\n')


class CaveIn:
    """
    >>> winds = '>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>'
    >>> cave = CaveIn(winds, 7)
    >>> for _ in range(2022): cave.drop_rock()
    >>> cave.top_height + 1
    3068
    >>> cave2 = CaveIn(winds, 7)
    >>> cave2.drop_rock_loop(2022)
    >>> cave2.top_height + 1
    3068
    >>> cave3 = CaveIn(winds, 7)
    >>> cave3.drop_rock_loop(1000000000000)
    >>> cave3.top_height + 1
    1514285714288
    """
    def __init__(self, wind: str, cave_width: int=7) -> None:
        self.wind = cycle(wind)
        self.cave_width = int(cave_width)
        shapes = [HorizontalLine, Cross, Angle, VerticalLine, Block]
        self._n_shapes = len(shapes)
        self.shapes = cycle(shapes)
        self.top_height = -1
        self.settled = set()
        self.n_fallen = 0

    def blocked_right(self, shape: RockShape) -> bool:
        if shape.right_edge() >= self.cave_width - 1:
            return True
        return any((x + 1, y) in self.settled for x, y in shape.coords)

    def blocked_left(self, shape: RockShape) -> bool:
        if shape.left_edge() <= 0:
            return True
        return any((x - 1, y) in self.settled for x, y in shape.coords)

    def blocked_bottom(self, shape: RockShape) -> bool:
        if shape.bottom_edge() <= 0:
            return True
        return any((x, y - 1) in self.settled for x, y in shape.coords)

    def drop_rock(self):
        shape = next(self.shapes)
        rock = shape(bottom_edge=self.top_height + 4, left_edge=2)
        while True:
            wind_char = next(self.wind)
            if wind_char == '<' and not self.blocked_left(rock):
                rock.move_side(-1)
            elif wind_char == '>' and not self.blocked_right(rock):
                rock.move_side(1)
            if not self.blocked_bottom(rock):
                rock.move_down(1)
            else:
                self.settled.update(rock.coords)
                self.top_height = max(self.top_height, rock.top_edge())
                self.n_fallen += 1
                break

    def settled_top_layer(self):
        floor = set((jj, -1) for jj in range(self.cave_width))
        if self.n_fallen == 0:
            return frozenset(floor)
        reachable = set((ii, self.top_height + 1) for ii in range(self.cave_width))
        surface = set()
        solids = self.settled.union(floor)
        while reachable:
            next_reach = set()
            for x, y in reachable:
                spreads_to = set([(x, y - 1)])
                if x > 0 and (x - 1, y) not in solids:
                    spreads_to.add((x - 1, y - 1))
                if x < self.cave_width - 1 and (x + 1, y) not in solids:
                    spreads_to.add((x + 1, y - 1))
                for pair in spreads_to:
                    if pair in solids:
                        surface.add(pair)
                    else:
                        next_reach.add(pair)
            reachable = next_reach
        return frozenset(surface)

    def drop_rock_loop(self, n_rocks: int):
        top_layer_cache = dict()
        remain = 0
        for nn in range(n_rocks):
            top = self.settled_top_layer()
            top_min = min(y for x, y in top)
            top = frozenset((x, y - top_min) for x, y in top)
            dropped, height = top_layer_cache.get(top, (None, None))
            if dropped is not None and ((nn - dropped) % self._n_shapes) == 0:
                drop_diff = nn - dropped
                height_diff = self.top_height - height
                repeats, remain = divmod(n_rocks - dropped, drop_diff)
                repeat_height = height + repeats * height_diff
                top_max = max(y for x, y in top)
                raise_dist = repeat_height - top_max
                raised_top = ((x, y + raise_dist) for x, y in top)
                self.settled.update(raised_top)
                self.top_height = repeat_height
                break
            else:
                top_layer_cache[top] = (nn, self.top_height)
                self.drop_rock()
        for _ in range(remain):
            self.drop_rock()


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day17.txt') as infile:
        winds = infile.readline().strip()

    # Part 1
    cave1 = CaveIn(winds)
    for _ in range(2022): cave1.drop_rock()
    print(cave1.top_height + 1)

    # Part 2
    cave2 = CaveIn(winds)
    cave2.drop_rock_loop(100000000000)
    print(cave2.top_height + 1)
