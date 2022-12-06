"""https://adventofcode.com/2022/day/5"""

def parse_diagram(lines):
    lines = list(lines)
    n_stacks = int(lines[-1].strip().split(' ')[-1])
    stacks = [list() for _ in range(n_stacks)]
    for line in lines[:-1]:
        for ii, char in enumerate(line[1:-1:4]):
            if char != ' ':
                stacks[ii].append(char)
    for jj in range(n_stacks):
        stacks[jj].reverse()
    return stacks


def print_stacks(stacks):
    lines = [' '.join(f' {ii + 1} ' for ii in range(len(stacks)))]
    max_height = max(len(ss) for ss in stacks)
    for jj in range(max_height):
        boxes = [
            '   ' if len(ss) <= jj else f'[{ss[jj]}]'
            for ss in stacks
        ]
        lines.append(' '.join(boxes))
    lines.reverse()
    print('\n'.join(lines))


def move_crates(stack_lines, direction_lines, model_9000=True):
    """
    >>> slines = [
    ...     '    [D]    ',
    ...     '[N] [C]    ',
    ...     '[Z] [M] [P]',
    ...     ' 1   2   3',
    ... ]
    >>> dlines = [
    ...     'move 1 from 2 to 1',
    ...     'move 3 from 1 to 3',
    ...     'move 2 from 2 to 1',
    ...     'move 1 from 1 to 2',
    ... ]
    >>> res = move_crates(slines, dlines)
    >>> ''.join(rr[-1] for rr in res)
    'CMZ'
    """
    stacks = parse_diagram(stack_lines)
    for line in direction_lines:
        # "move (number) from (from_stack) to (to_stack)"
        _, number, _, from_stack, _, to_stack = line.strip().split(' ')
        number = int(number)
        from_stack = int(from_stack) - 1
        to_stack = int(to_stack) - 1
        moved = stacks[from_stack][-number:]
        del stacks[from_stack][-number:]
        if model_9000:
            moved.reverse()
        stacks[to_stack].extend(moved)
    return stacks


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    with open('day05.txt') as infile:
        stack_lines = []
        direction_lines = []
        reading_stack = True
        for line in infile:
            line = line.replace('\n', '')
            if line.strip() == '':
                reading_stack = False
            elif reading_stack:
                stack_lines.append(line)
            else:
                direction_lines.append(line)
    # Part1
    stacks1 = move_crates(stack_lines, direction_lines)
    print(''.join(ss[-1] for ss in stacks1))
    # Part2
    stacks2 = move_crates(stack_lines, direction_lines, model_9000=False)
    print(''.join(ss[-1] for ss in stacks2))
