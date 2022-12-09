"""https://adventofcode.com/2022/day/9"""

def is_adjacent(p1, p2):
    return abs(p1[0] - p2[0]) <= 1 and abs(p1[1] - p2[1]) <= 1


def unit_diff(x1: int, x2: int) -> int:
    diff = x2 - x1
    if diff == 0:
        return 0
    return -1 if diff < 0 else 1


class Rope:
    def __init__(self, length=2):
        self.parts = [(0, 0)] * length
        self.tail_path = set([self.parts[-1]])

    def _part_follow(self, index):
        follower, leader = self.parts[index], self.parts[index - 1]
        if not is_adjacent(follower, leader):
            self.parts[index] = tuple(
                tt + unit_diff(tt, hh)
                for tt, hh in zip(follower, leader)
            )

    def follow_instruction(self, text):
        """
        >>> r = Rope()
        >>> commands = ['R 4', 'U 4', 'L 3', 'D 1', 'R 4', 'D 1', 'L 5', 'R 2']
        >>> for com in commands: r.follow_instruction(com)
        >>> len(r.tail_path)
        13
        >>> r = Rope(10)
        >>> for com in commands: r.follow_instruction(com)
        >>> len(r.tail_path)
        1
        """
        direction, steps = text.strip().split(' ')
        steps = int(steps)
        if direction == 'R':
            for _ in range(steps):
                self.parts[0] = (self.parts[0][0] + 1, self.parts[0][1])
                for ii in range(1, len(self.parts)):
                    self._part_follow(ii)
                self.tail_path.add(self.parts[-1])
        if direction == 'L':
            for _ in range(steps):
                self.parts[0] = (self.parts[0][0] - 1, self.parts[0][1])
                for ii in range(1, len(self.parts)):
                    self._part_follow(ii)
                self.tail_path.add(self.parts[-1])
        if direction == 'U':
            for _ in range(steps):
                self.parts[0] = (self.parts[0][0], self.parts[0][1] + 1)
                for ii in range(1, len(self.parts)):
                    self._part_follow(ii)
                self.tail_path.add(self.parts[-1])
        if direction == 'D':
            for _ in range(steps):
                self.parts[0] = (self.parts[0][0], self.parts[0][1] - 1)
                for ii in range(1, len(self.parts)):
                    self._part_follow(ii)
                self.tail_path.add(self.parts[-1])


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    with open('day09.txt') as infile:
        lines = [line.replace('\n', '') for line in infile]
    # Part 1
    rope = Rope()
    for command in lines:
        rope.follow_instruction(command)
    print(len(rope.tail_path))
    # Part 2
    long_rope = Rope(10)
    for command in lines:
        long_rope.follow_instruction(command)
    print(len(long_rope.tail_path))
