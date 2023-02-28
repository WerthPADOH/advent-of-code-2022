"""https://adventofcode.com/2022/day/21"""
import collections
import operator


class Monkey:
    def __init__(self, value=None, op=None, needs=None, feeds=None):
        self.value = value
        self.op = op
        if needs is None:
            needs = []
        self.needs = list(needs)
        if feeds is None:
            feeds = []
        self.feeds = list(feeds)
        self._in_values = dict()

    def __repr__(self) -> str:
        op_str = {
            operator.add: 'operator.add', operator.sub: 'operator.sub',
            operator.mul: 'operator.mul', operator.floordiv: 'operator.floordiv',
            None: 'None',
        }
        return f'Monkey(value={self.value}, op={op_str[self.op]}, needs={self.needs}, feeds={self.feeds})'

    def feed_value(self, in_name, in_value):
        if not self._in_values:
            self._in_values = dict.fromkeys(self.needs, None)
        self._in_values[in_name] = in_value
        if all(val is not None for val in self._in_values.values()):
            a = self._in_values[self.needs[0]]
            b = self._in_values[self.needs[1]]
            self.value = self.op(a, b)

    def backfeed_result(self):
        """
        >>> m = Monkey()
        >>> m.needs = ['A', 'B']
        >>> m.op = operator.floordiv
        >>> m.feed_value('A', 21)
        >>> m.value = 7
        >>> m.backfeed_result()
        {'A': 21, 'B': 3}
        """
        try:
            name_a, name_b = self.needs
        except ValueError:
            print(self)
            raise
        missing_in = sum(value is None for value in self._in_values.values())
        if missing_in != 1:
            return self._in_values
        unknown_a = self._in_values[name_a] is None
        if self.op is operator.add:
            if unknown_a:
                self._in_values[name_a] = self.value - self._in_values[name_b]
                return self._in_values
            else:
                self._in_values[name_b] = self.value - self._in_values[name_a]
                return self._in_values
        elif self.op is operator.sub:
            if unknown_a:
                self._in_values[name_a] = self.value + self._in_values[name_b]
                return self._in_values
            else:
                self._in_values[name_b] = self._in_values[name_a] - self.value
                return self._in_values
        elif self.op is operator.mul:
            if unknown_a:
                self._in_values[name_a] = self.value // self._in_values[name_b]
                return self._in_values
            else:
                self._in_values[name_b] = self.value // self._in_values[name_a]
                return self._in_values
        elif self.op is operator.floordiv:
            if unknown_a:
                self._in_values[name_a] = self.value * self._in_values[name_b]
                return self._in_values
            else:
                self._in_values[name_b] = self._in_values[name_a] // self.value
                return self._in_values


class MonkeyChain:
    """
    >>> lines = [
    ...     'root: pppw + sjmn',
    ...     'dbpl: 5',
    ...     'cczh: sllz + lgvd',
    ...     'zczc: 2',
    ...     'ptdq: humn - dvpt',
    ...     'dvpt: 3',
    ...     'lfqf: 4',
    ...     'humn: 5',
    ...     'ljgn: 2',
    ...     'sjmn: drzm * dbpl',
    ...     'sllz: 4',
    ...     'pppw: cczh / lfqf',
    ...     'lgvd: ljgn * ptdq',
    ...     'drzm: hmdt - zczc',
    ...     'hmdt: 32',
    ... ]
    >>> mc = MonkeyChain(lines)
    >>> mc.solve()
    >>> mc.monkeys['root'].value
    152
    >>> mc = MonkeyChain(lines)
    >>> mc.backsolve()
    >>> mc.monkeys['humn'].value
    301
    """
    def __init__(self, lines):
        self.monkeys = collections.defaultdict(Monkey)
        self.valued = set()
        op_signs = {
            '+': operator.add, '-': operator.sub,
            '*': operator.mul, '/': operator.floordiv
        }
        for line in lines:
            line = line.strip()
            name, info = line.split(': ')
            if info.isdigit():
                self.monkeys[name].value = int(info)
            else:
                name_a, sign, name_b = info.split(' ')
                self.monkeys[name].op = op_signs[sign]
                self.monkeys[name].needs = (name_a, name_b)
                self.monkeys[name_a].feeds.append(name)
                self.monkeys[name_b].feeds.append(name)

    def solve(self):
        unsolved = set(self.monkeys.keys())
        while unsolved:
            new_unsolved = set()
            for name in unsolved:
                monkey = self.monkeys[name]
                if monkey.value is not None:
                    for fed_name in monkey.feeds:
                        fed_monkey = self.monkeys[fed_name]
                        fed_monkey.feed_value(name, monkey.value)
                else:
                    new_unsolved.add(name)
            if unsolved == new_unsolved:
                break
            unsolved = new_unsolved


    def backsolve(self):
        self.monkeys['root'].op = operator.eq
        self.monkeys['humn'].value = None
        self.solve()
        need_backsolved = set()
        root = self.monkeys['root']
        root_value = None
        for in_name in root.needs:
            if self.monkeys[in_name].value is not None:
                root_value = self.monkeys[in_name].value
        for in_name in root.needs:
            if self.monkeys[in_name].value is None:
                self.monkeys[in_name].value = root_value
                need_backsolved.add(in_name)
        while need_backsolved:
            name = need_backsolved.pop()
            monkey = self.monkeys[name]
            in_values = monkey.backfeed_result()
            for in_name, value in in_values.items():
                in_monkey = self.monkeys[in_name]
                if in_monkey.value is None and value is not None:
                    in_monkey.value = value
                    if in_name != 'humn':
                        need_backsolved.add(in_name)


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day21.txt') as infile:
        inlines = [line.strip() for line in infile]

    # Part 1
    chain = MonkeyChain(inlines)
    chain.solve()
    print(chain.monkeys['root'].value)

    # Part 2
    chain = MonkeyChain(inlines)
    chain.backsolve()
    print(chain.monkeys['humn'].value)
