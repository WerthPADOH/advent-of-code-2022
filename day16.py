"""https://adventofcode.com/2022/day/16"""
# I'm doing this with my work laptop, which only has Python 3.7, so no graphlib
import re
from dataclasses import dataclass
from typing import Dict, Iterable, List


@dataclass
class Valve:
    name: str
    flow: int
    connections: frozenset

    def __hash__(self) -> int:
        return hash((type(self), self.name, self.flow, self.connections))


@dataclass
class Path:
    steps: List[str]
    released: int=0
    duration: int=0

    def copy(self):
        return Path(self.steps.copy(), self.released, self.duration)


class TunnelNetwork:
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
    >>> nw = TunnelNetwork.parse(text)
    >>> nw.valves['AA'].name, nw.valves['AA'].flow
    ('AA', 0)
    >>> nw.valves['AA'].connections == {'DD', 'BB', 'II'}
    True
    >>> nw.valves['HH'].name, nw.valves['HH'].flow
    ('HH', 22)
    >>> nw.valves['HH'].connections == {'GG'}
    True
    >>> nw.distances[frozenset(('AA', 'BB'))]
    1
    >>> nw.distances[frozenset(('AA', 'EE'))]
    2
    >>> nw.distances[frozenset(('HH', 'JJ'))]
    7
    >>> nw.flow_valves == {'BB', 'CC', 'DD', 'EE', 'HH', 'JJ'}
    True
    >>> nw.max_release(30)
    1651
    """
    def __init__(self) -> None:
        self.valves: Dict[str, Valve] = dict()
        self.flow_valves: frozenset[str] = frozenset()
        self.distances: Dict[frozenset, int] = dict()

    @classmethod
    def parse(self, stream: Iterable[str]):
        tn = TunnelNetwork()
        pattern = re.compile(
            r'Valve (?P<name>[A-Z]+) has flow '
            r'rate=(?P<flow>\d+); '
            r'tunnel(s)? lead(s)? to valve(s)? (?P<cons>([A-Z]+, )*[A-Z]+)'
        )
        flowing = set()
        for line in stream:
            line = line.strip()
            m = pattern.match(line)
            if m:
                name = m.group('name')
                flow = int(m.group('flow'))
                cons = frozenset(m.group('cons').split(', '))
                tn.valves[name] = Valve(name, flow, cons)
                if flow > 0:
                    flowing.add(name)
        tn.flow_valves = frozenset(flowing)
        for start in tn.valves.values():
            current_round = start.connections
            visited = set()
            dist = 0
            while current_round:
                dist += 1
                next_round = set()
                for end in current_round:
                    visited.add(end)
                    pair = frozenset((start.name, end))
                    if pair not in tn.distances:
                        tn.distances[pair] = dist
                    next_round.update(
                        next_con for next_con in tn.valves[end].connections
                        if next_con not in visited
                    )
                current_round = next_round
        return tn

    def _recursive_flow_path(self, path: Path, max_time: int) -> Iterable[Path]:
        options = self.flow_valves.difference(path.steps)
        if len(options) == 0:
            yield path
        for opt in options:
            dist = self.distances[frozenset((path.steps[-1], opt))]
            time_to_open = dist + 1
            if path.duration + time_to_open >= max_time:
                yield path
            else:
                next_path = path.copy()
                next_path.steps.append(opt)
                next_path.duration += time_to_open
                flow = self.valves[opt].flow
                next_path.released += flow * (max_time - next_path.duration)
                yield from self._recursive_flow_path(next_path, max_time)

    def max_release(self, time: int, start: str='AA') -> int:
        # Only looking at the order of valve releases in paths
        best_path = Path([])
        for path in self._recursive_flow_path(Path([start]), max_time=time):
            if path.released > best_path.released:
                best_path = path
        return best_path.released


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day16.txt') as infile:
        network = TunnelNetwork.parse(infile.readlines())

    # Part 1
    print(network.max_release(30))
