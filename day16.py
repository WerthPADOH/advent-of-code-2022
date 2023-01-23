"""https://adventofcode.com/2022/day/16"""
# I'm doing this with my work laptop, which only has Python 3.7, so no graphlib
import re
from dataclasses import dataclass
from typing import Dict, Iterable, Set


@dataclass
class Valve:
    name: str
    flow: int
    connections: frozenset

    def __hash__(self) -> int:
        return hash((type(self), self.name, self.flow, self.connections))


def parse_network(stream) -> Dict[str, Valve]:
    """
    >>> text = [
    ...     'Valve AA has flow rate=0; tunnels lead to valves DD, II, BB',
    ...     'Valve HH has flow rate=22; tunnel leads to valve GG'
    ... ]
    >>> nw = parse_network(text)
    >>> nw['AA'].name, nw['AA'].flow
    ('AA', 0)
    >>> nw['AA'].connections == {'DD', 'BB', 'II'}
    True
    >>> nw['HH'].name, nw['HH'].flow
    ('HH', 22)
    >>> nw['HH'].connections == {'GG'}
    True
    """
    pattern = re.compile(
        r'Valve (?P<name>[A-Z]+) has flow '
        r'rate=(?P<flow>\d+); '
        r'tunnel(s)? lead(s)? to valve(s)? (?P<cons>([A-Z]+, )*[A-Z]+)'
    )
    out = dict()
    for line in stream:
        line = line.strip()
        m = pattern.match(line)
        if m:
            name = m.group('name')
            flow = int(m.group('flow'))
            cons = frozenset(m.group('cons').split(', '))
            out[name] = Valve(name, flow, cons)
    return out


class Path:
    """
    >>> p = Path(['a', 'b', 'c'])
    >>> p.latest()
    'c'
    >>> p2 = p.open_last()
    >>> p2.latest()
    'c'
    """
    def __init__(self, actions: Iterable) -> None:
        self.actions = tuple(actions)
        self.opened = set()

    def move_to(self, destination: str):
        new_path = Path(self.actions + (destination, ))
        new_path.opened = self.opened.copy()
        return new_path

    def open_last(self):
        new_path = Path(self.actions + ('open', ))
        new_path.opened = self.opened.copy()
        new_path.opened.add(self.actions[-1])
        return new_path

    def latest(self) -> str:
        if self.actions[-1] == 'open':
            return self.actions[-2]
        else:
            return self.actions[-1]

    def loop_valves(self) -> Set[str]:
        loopers = set()
        for action in reversed(self.actions):
            if action == 'open':
                break
            loopers.add(action)
        return loopers

    def released(self, network: Dict[str, Valve]) -> int:
        total = 0
        for ii, act in enumerate(self.actions):
            if act == 'open':
                time_left = len(self.actions) - ii - 1
                opened_valve = self.actions[ii - 1]
                flow = network[opened_valve].flow
                total += time_left * flow
        return total

    def __len__(self) -> int:
        return len(self.actions)

    def __repr__(self) -> str:
        return 'Path((' + ', '.join(repr(act) for act in self.actions) + '))'


def all_paths(traveled: Path, network: Dict[str, Valve], time: int):
    """
    >>> text = [
    ...     'Valve AA has flow rate=0; tunnels lead to valves DD, II, BB',
    ...     'Valve BB has flow rate=13; tunnels lead to valves CC, AA',
    ...     'Valve CC has flow rate=2; tunnels lead to valves DD, BB',
    ...     'Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE',
    ...     'Valve EE has flow rate=3; tunnels lead to valves FF, DD',
    ...     'Valve FF has flow rate=0; tunnels lead to valves EE, GG',
    ...     'Valve GG has flow rate=0; tunnels lead to valves FF, HH',
    ...     'Valve HH has flow rate=22; tunnel leads to valve GG',
    ...     'Valve II has flow rate=0; tunnels lead to valves AA, JJ',
    ...     'Valve JJ has flow rate=21; tunnel leads to valve II',
    ... ]
    >>> nw = parse_network(text)
    >>> for p in sorted(x.actions for x in all_paths(('AA', ), nw, 3)): print(p)
    ('AA', 'BB', 'CC')
    ('AA', 'DD', 'CC')
    ('AA', 'DD', 'EE')
    ('AA', 'II', 'JJ')
    """
    if not isinstance(traveled, Path):
        traveled = Path(traveled)
    if len(traveled) == time:
        yield traveled
        return
    else:
        latest = traveled.latest()
        # Avoid going in a loop without opening any valves
        for con in network[latest].connections - traveled.loop_valves():
            new_path = traveled.move_to(con)
            yield from all_paths(new_path, network, time)
        # Don't bother to open valves in the last step when there's no flow time
        # And don't bother to open valves with 0 flow
        time_to_flow = time - len(traveled) > 1
        positive_flow = network[latest].flow > 0
        if time_to_flow and positive_flow and latest not in traveled.opened:
            opening_path = traveled.open_last()
            for con in network[latest].connections:
                new_path = opening_path.move_to(con)
                yield from all_paths(new_path, network, time)


def most_released(network: Dict[str, Valve], time: int, start: str='AA') -> int:
    """
    >>> text = [
    ...     'Valve AA has flow rate=0; tunnels lead to valves DD, II, BB',
    ...     'Valve BB has flow rate=13; tunnels lead to valves CC, AA',
    ...     'Valve CC has flow rate=2; tunnels lead to valves DD, BB',
    ...     'Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE',
    ...     'Valve EE has flow rate=3; tunnels lead to valves FF, DD',
    ...     'Valve FF has flow rate=0; tunnels lead to valves EE, GG',
    ...     'Valve GG has flow rate=0; tunnels lead to valves FF, HH',
    ...     'Valve HH has flow rate=22; tunnel leads to valve GG',
    ...     'Valve II has flow rate=0; tunnels lead to valves AA, JJ',
    ...     'Valve JJ has flow rate=21; tunnel leads to valve II',
    ... ]
    >>> nw = parse_network(text)
    >>> p = most_released(nw, time=5)
    >>> list(p.actions)[:5]
    ['AA', 'DD', 'open', 'EE', 'open']
    >>> p.released(nw)
    63
    >>> nw = parse_network(text)
    >>> p = most_released(nw, time=30)
    >>> p.released(nw)
    1651
    """
    base_path = Path([start])
    best_path = base_path
    best_released = best_path.released(network)
    for path in all_paths(base_path, network, time + 1):
        released = path.released(network)
        if released > best_released:
            best_path = path
            best_released = released
    return best_path


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day16.txt') as infile:
        network = parse_network(infile.readlines())

    # Part 1
    top_path = most_released(network, time=30, start='AA')
