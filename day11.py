"""https://adventofcode.com/2022/day/11"""

import operator
from collections import defaultdict
from dataclasses import dataclass
from typing import List
from types import FunctionType


def make_worry_test(divisor: int, if_true: int, if_false: int) -> FunctionType:
    """
    >>> f = make_worry_test(4, 101, 102)
    >>> [f(x) for x in range(10)]
    [101, 102, 102, 102, 101, 102, 102, 102, 101, 102]
    """
    def worry_test(worry: int) -> int:
        if (worry % divisor) == 0:
            return if_true
        else:
            return if_false
    return worry_test


def make_operator(token1: str, token2: str, operator: FunctionType) -> FunctionType:
    if token1 == 'old' and token2 == 'old':
        def operation(old):
            return operator(old, old)
    elif token1 == 'old' and token2 != 'old':
        t2_val = int(token2)
        def operation(old):
            return operator(old, t2_val)
    elif token1 != 'old' and token2 == 'old':
        t1_val = int(token1)
        def operation(old):
            return operator(t1_val, old)
    return operation


class Monkey:
    def __init__(
        self,
        items: List=None,
        operation: FunctionType=None,
        test: FunctionType=None,
    ):
        self.items = [] if items is None else list(items)
        if operation is not None:
            self.operation = operation
        self.test = test

    def operation(self, old: int) -> int:
        return old

    @classmethod
    def from_lines(self, lines):
        """
        >>> text = [
        ...     'Monkey 1:',
        ...     'Starting items: 54, 65, 75, 74',
        ...     'Operation: new = old + 6',
        ...     'Test: divisible by 19',
        ...     '    If true: throw to monkey 2',
        ...     '    If false: throw to monkey 0',
        ... ]
        >>> m = Monkey.from_lines(text)
        >>> m.items
        [54, 65, 75, 74]
        >>> m.operation(54)
        60
        >>> m.test(152), m.test(20)
        (2, 0)
        """
        op_map = {'+': operator.add, '*': operator.mul}
        monkey = Monkey()
        div = None
        if_t = None
        if_f = None
        for line in lines:
            line = line.strip()
            if line.startswith('Starting items:'):
                item_list = line.rpartition(': ')[-1]
                monkey.items = [int(value) for value in item_list.split(',')]
            elif line.startswith('Operation:'):
                expression = line.rpartition('= ')[-1]
                token1, op_token, token2 = expression.split(' ')
                op = op_map[op_token]
                monkey.operation = make_operator(token1, token2, op)
            elif line.startswith('Test'):
                div = int(line.rpartition(' ')[-1])
            elif line.startswith('If true:'):
                if_t = int(line.rpartition(' ')[-1])
            elif line.startswith('If false:'):
                if_f = int(line.rpartition(' ')[-1])
        monkey.test = make_worry_test(div, if_t, if_f)
        return monkey


class MonkeyGame:
    """
    >>> monkey_specs = [
    ...    [
    ...        'Monkey 0:',
    ...        '    Starting items: 79, 98',
    ...        '    Operation: new = old * 19',
    ...        '    Test: divisible by 23',
    ...        '        If true: throw to monkey 2',
    ...        '        If false: throw to monkey 3',
    ...    ],
    ...    [
    ...        'Monkey 1:',
    ...        '    Starting items: 54, 65, 75, 74',
    ...        '    Operation: new = old + 6',
    ...        '    Test: divisible by 19',
    ...        '        If true: throw to monkey 2',
    ...        '        If false: throw to monkey 0',
    ...    ],
    ...    [
    ...        'Monkey 2:',
    ...        '    Starting items: 79, 60, 97',
    ...        '    Operation: new = old * old',
    ...        '    Test: divisible by 13',
    ...        '        If true: throw to monkey 1',
    ...        '        If false: throw to monkey 3',
    ...    ],
    ...    [
    ...        'Monkey 3:',
    ...        '    Starting items: 74',
    ...        '    Operation: new = old + 3',
    ...        '    Test: divisible by 17',
    ...        '        If true: throw to monkey 0',
    ...        '        If false: throw to monkey 1',
    ...    ],
    ... ]
    >>> m = [Monkey.from_lines(bunch) for bunch in monkey_specs]
    >>> mg = MonkeyGame(m)
    >>> mg.progress_round()
    >>> mg.monkeys[0].items
    [20, 23, 27, 26]
    >>> mg.monkeys[1].items
    [2080, 25, 167, 207, 401, 1046]
    >>> mg.monkeys[2].items, mg.monkeys[3].items
    ([], [])
    >>> for _ in range(19): mg.progress_round()
    >>> mg.inspections
    [101, 95, 7, 105]
    >>> mg.monkey_business()
    10605
    >>> m = [Monkey.from_lines(bunch) for bunch in monkey_specs]
    >>> mg = MonkeyGame(m, calm_factor=1)
    >>> for _ in range(10000): mg.progress_round()
    >>> mg.monkey_business()
    2713310158
    """
    def __init__(self, monkeys: List[Monkey], calm_factor: int=3):
        self.monkeys = monkeys
        self.inspections = [0] * len(self.monkeys)
        self.calm_factor = int(calm_factor)

    def progress_round(self):
        for nn in range(len(self.monkeys)):
            monk = self.monkeys[nn]
            items = monk.items.copy()
            monk.items.clear()
            for it in items:
                self.inspections[nn] += 1
                it = monk.operation(it)
                if self.calm_factor != 1:
                    it = it // self.calm_factor
                recipient = monk.test(it)
                try:
                    self.monkeys[recipient].items.append(it)
                except IndexError:
                    print(f'Thrower: {nn}, Catcher: {recipient}, Item: {it}, Monkey count: {len(self.monkeys)}')
                    raise

    def monkey_business(self):
        most_active = list(sorted(self.inspections))[-2:]
        return most_active[0] * most_active[1]


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day11.txt') as infile:
        lines = infile.readlines()
    # Part 1
    line_feed = lines.copy()
    monkeys = []
    while line_feed:
        if line_feed[0].startswith('Monkey'):
            monkeys.append(Monkey.from_lines(line_feed[1:7]))
            del line_feed[:7]
        else:
            del line_feed[0]
    game = MonkeyGame(monkeys)
    for _ in range(20):
        game.progress_round()
    print(game.monkey_business())
