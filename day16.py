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


class ValvePath:
    def __init__(self, actions: Iterable=tuple(), released: int=0) -> None:
        self.actions = list(actions)
        self.released = int(released)
        self.opened_valves = set()
        if self.actions:
            for ii, act in enumerate(self.actions[1:], 1):
                if act == 'open':
                    self.opened_valves.add(self.actions[ii - 1])

    def copy(self):
        new_vp = ValvePath()
        new_vp.actions = self.actions.copy()
        new_vp.released = self.released
        new_vp.opened_valves = self.opened_valves.copy()
        return new_vp

    def move(self, location: str):
        self.actions.append(location)

    def open_valve(self, valve: Valve):
        self.actions.append('open')
        self.opened_valves.add(valve.name)
        self.released = valve.flow * (30 - len(self.actions))

    def current_location(self) -> str:
        if self.actions[-1] == 'open':
            return self.actions[-2]
        return self.actions[-1]


def path_max_release(network: Dict[str, Valve]) -> ValvePath:
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
    >>> p = path_max_release(nw)
    >>> p.released
    1651
    """
    running = [ValvePath([room]) for room in network['AA'].connections]
    best_path = ValvePath(['AA'])
    while running:
        path = running.pop()
        # Keep a finished path if it's the best, otherwise ignore
        if len(path.actions) == 30 and path.released > best_path.released:
            best_path = path
            continue
        # Only continue paths that can still win
        n_openable = (31 - len(path.actions)) // 2
        flows_left = [
            vv.flow for name, vv in network.items()
            if name not in path.opened_valves
        ]
        flows_left.sort(reverse=True)
        possible = sum(flows_left[:n_openable]) + path.released
        if possible > best_path.released:
            location = path.current_location()
            current_valve = network[location]
            if not location in path.opened_valves:
                new_path = path.copy()
                new_path.open_valve(current_valve)
                running.append(new_path)
            cons = current_valve.connections
            for valve in cons:
                new_path = path.copy()
                new_path.move(valve)
                running.append(new_path)
    return best_path


if __name__ == '__main__':
    import doctest
    doctest.testmod()
