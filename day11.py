"""https://adventofcode.com/2022/day/11"""

import functools
import operator
from typing import List
from types import FunctionType


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
        items: List,
        operation: FunctionType,
        test_div: int,
        test_true_dest: int,
        test_false_dest: int,
    ):
        self.items = list(items)
        self.operation = operation
        self.test_div = test_div
        self.test_true_dest = test_true_dest
        self.test_false_dest = test_false_dest

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
        div = None
        if_t = None
        if_f = None
        for line in lines:
            line = line.strip()
            if line.startswith('Starting items:'):
                item_list = line.rpartition(': ')[-1]
                items = [int(value) for value in item_list.split(',')]
            elif line.startswith('Operation:'):
                expression = line.rpartition('= ')[-1]
                token1, op_token, token2 = expression.split(' ')
                op = op_map[op_token]
                operation = make_operator(token1, token2, op)
            elif line.startswith('Test'):
                div = int(line.rpartition(' ')[-1])
            elif line.startswith('If true:'):
                if_t = int(line.rpartition(' ')[-1])
            elif line.startswith('If false:'):
                if_f = int(line.rpartition(' ')[-1])
        return Monkey(
            items=items, operation=operation,
            test_div=div, test_true_dest=if_t, test_false_dest=if_f
        )

    def test(self, value) -> int:
        if (value % self.test_div) == 0:
            return self.test_true_dest
        else:
            return self.test_false_dest


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
    >>> mg = MonkeyGame(m, calm_factor=3)
    >>> mg.progress_round(n_rounds=20)
    >>> mg.inspections
    [101, 95, 7, 105]
    >>> mg.monkey_business()
    10605
    >>> mg = MonkeyGame(m, calm_factor=3)
    >>> mg.item_driven_rounds(n_rounds=20)
    >>> mg.inspections
    [101, 95, 7, 105]
    >>> mg.monkey_business()
    10605
    >>> mg = MonkeyGame(m, calm_factor=1)
    >>> mg.item_driven_rounds(n_rounds=10000)
    >>> mg.monkey_business()
    2713310158
    """
    def __init__(self, monkeys: List[Monkey], calm_factor: int=3):
        self.monkeys = list(monkeys)
        self._n_monkeys = len(self.monkeys)
        self._items = []
        self._owners = []
        for ii, monk in enumerate(self.monkeys):
            self._items.extend(monk.items)
            self._owners.extend([ii] * len(monk.items))
        self.inspections = [0] * self._n_monkeys
        self.calm_factor = int(calm_factor)

    def progress_round(self, n_rounds: int=1):
        big_div = functools.reduce(
            operator.mul,
            (monk.test_div for monk in self.monkeys)
        )
        for _ in range(n_rounds):
            for nn in range(self._n_monkeys):
                monk = self.monkeys[nn]
                for ii, (owner, val) in enumerate(zip(self._owners, self._items)):
                    if owner == nn:
                        self.inspections[nn] += 1
                        val = monk.operation(val)
                        if self.calm_factor != 1:
                            val = val // self.calm_factor
                        else:
                            val = val % big_div
                        recipient = monk.test(val)
                        self._items[ii] = val
                        self._owners[ii] = recipient

    def item_driven_rounds(self, n_rounds: int):
        big_div = functools.reduce(
            operator.mul,
            (monk.test_div for monk in self.monkeys)
        )
        for item, owner in zip(self._items, self._owners):
            elapsed = 0
            while elapsed < n_rounds:
                monk = self.monkeys[owner]
                self.inspections[owner] += 1
                item = monk.operation(item)
                if self.calm_factor != 1:
                    item = item // self.calm_factor
                else:
                    item = item % big_div
                recipient = monk.test(item)
                if recipient < owner:
                    elapsed += 1
                owner = recipient

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
    game = MonkeyGame(monkeys, calm_factor=3)
    game.progress_round(n_rounds=20)
    print(game.monkey_business())

    # Part 2
    line_feed = lines.copy()
    monkeys = []
    while line_feed:
        if line_feed[0].startswith('Monkey'):
            monkeys.append(Monkey.from_lines(line_feed[1:7]))
            del line_feed[:7]
        else:
            del line_feed[0]
    game = MonkeyGame(monkeys, calm_factor=1)
    game.item_driven_rounds(n_rounds=10000)
    print(game.monkey_business())
