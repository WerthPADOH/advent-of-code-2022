"""https://adventofcode.com/2022/day/16"""
# I'm doing this with my work laptop, which only has Python 3.7, so no graphlib
import re
from collections import deque
from dataclasses import dataclass
from typing import Dict, Iterable, List


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


class ReversePath:
    def __init__(self) -> None:
        self.actions = deque([])
        self.released = 0
        self.opened_valves = set()

    def copy(self):
        new_vp = ReversePath()
        new_vp.actions = self.actions.copy()
        new_vp.released = self.released
        new_vp.opened_valves = self.opened_valves.copy()
        return new_vp

    def move(self, valve: Valve):
        self.actions.appendleft(valve.name)

    def move_and_open(self, valve: Valve):
        """
        >>> rp = ReversePath()
        >>> v = Valve('X', 7, [])
        >>> rp.move_and_open(v)
        >>> rp.released
        0
        >>> v2 = Valve('Y', 13, [])
        >>> rp.move_and_open(v2)
        >>> rp.released
        26
        """
        flowing_time = len(self.actions)
        self.actions.appendleft('open')
        self.actions.appendleft(valve.name)
        self.opened_valves.add(valve.name)
        self.released += valve.flow * flowing_time

    def current_location(self) -> str:
        return self.actions[0]

    def __str__(self) -> str:
        return ' -> '.join(self.actions)


def add_new_best_paths(
    old_paths: Iterable[ReversePath],
    new_paths: Iterable[ReversePath],
) -> List[ReversePath]:
    """
    >>> older = [ReversePath(), ReversePath()]
    >>> older[0].actions = ['BB', 'open', 'CC', 'open', 'DD']
    >>> older[0].released = (13 * 3) + (2 * 1)
    >>> older[1].actions = ['DD', 'open', 'CC', 'open', 'EE']
    >>> older[1].released = (20 * 3) + (2 * 1)
    >>> newer = [ReversePath(), ReversePath(), ReversePath()]
    >>> newer[0].actions = ['BB', 'open', 'CC', 'DD', 'open']
    >>> newer[0].released = (13 * 3) + (20 * 0)
    >>> newer[1].actions = ['DD', 'open', 'EE', 'open', 'FF']
    >>> newer[1].released = (20 * 3) + (3 * 1)
    >>> newer[2].actions = ['II', 'JJ', 'open', 'II', 'AA']
    >>> newer[2].released = 21 * 2
    >>> # Keep old BB-branch, replace DD-branch, add II-branch
    >>> res = add_new_best_paths(older, newer)
    >>> len(res) == 3
    True
    >>> print([r for r in res if r.current_location() == 'BB'][0])
    BB -> open -> CC -> open -> DD
    >>> print([r for r in res if r.current_location() == 'DD'][0])
    DD -> open -> EE -> open -> FF
    >>> print([r for r in res if r.current_location() == 'II'][0])
    II -> JJ -> open -> II -> AA
    """
    best = {(op.current_location(), len(op.actions)): op for op in old_paths}
    for np in new_paths:
        key = (np.current_location(), len(np.actions))
        champ = best.get(key)
        if champ is None or np.released > champ.released:
            best[key] = np
    return list(best.values())


def path_max_release(network: Dict[str, Valve], time: int=30) -> ReversePath:
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
    >>> p = path_max_release(nw, time=5)
    >>> list(p.actions)[:5]
    ['AA', 'DD', 'open', 'EE', 'open']
    >>> p.released
    63
    >>> nw = parse_network(text)
    >>> p = path_max_release(nw)
    >>> p.released
    1651
    """
    # Working backwards
    time = time + 1 # Account for minute 0 at AA
    backtracks = deque()
    for valve in network.values():
        tail = ReversePath()
        tail.move(valve)
        backtracks.append(tail)
    while len(backtracks) > 1:
        path = backtracks.pop()
        time_taken = len(path.actions)
        current_valve = network[path.current_location()]
        if time_taken > time or (time_taken == time and current_valve.name != 'AA'):
            continue
        if time_taken == time:
            backtracks.appendleft(path)
            continue
        added_tracks = []
        for con_name in current_valve.connections:
            to_valve = network[con_name]
            walk_only = path.copy()
            walk_only.move(to_valve)
            added_tracks.append(walk_only)
            unopened = con_name not in path.opened_valves
            has_flow = to_valve.flow > 0
            if unopened and has_flow and time - len(path.actions) >= 2:
                walk_open = path.copy()
                walk_open.move_and_open(to_valve)
                added_tracks.append(walk_open)
        backtracks = deque(add_new_best_paths(backtracks, added_tracks))
    return backtracks[0]


if __name__ == '__main__':
    import doctest
    doctest.testmod()
