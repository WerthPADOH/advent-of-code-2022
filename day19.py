"""https://adventofcode.com/2022/day/19"""
import re
from typing import List


class RoboRecipe:
    def __init__(self, output: str, cost: dict=None) -> None:
        self.output = str(output)
        self.cost = {'ore': 0, 'clay': 0, 'obsidian': 0, 'geode': 0}
        if cost is not None:
            self.cost.update(cost)

    def __repr__(self) -> str:
        nonzero_cost = {mat: num for mat, num in self.cost.items() if num > 0}
        return f"RoboRecipe('{self.output}', {repr(nonzero_cost)})"

    def affordable(self, resources: dict) -> bool:
        return all(
            self.cost[material] <= possessed
            for material, possessed in resources.items()
        )


class Blueprint:
    def __init__(self, text: str) -> None:
        label, details = text.split(': ')
        self.number = int(label[10:])
        self.recipes: List[RoboRecipe] = []
        detail_pattern = re.compile(
            r'Each (?P<output>\w+) robot costs '
            r'(?P<costs>\d+ \w+(?: and \d+ \w+)*)'
        )
        for robo_details in details.split('. '):
            pmatch = detail_pattern.match(robo_details)
            recipe = RoboRecipe(pmatch.group('output'))
            for cost_text in pmatch.group('costs').split(' and '):
                value, resource = cost_text.split(' ')
                recipe.cost[resource] = int(value)
            self.recipes.append(recipe)

    def get_recipe(self, resource: str) -> RoboRecipe:
        for recipe in self.recipes:
            if recipe.output == resource:
                return recipe
        raise ValueError(f'Blueprint has no RoboRecipe producing {resource}')

    def list_affordable(self, resources: dict) -> list:
        """
        >>> bp_text = (
        ...     'Blueprint 1: '
        ...     'Each ore robot costs 4 ore. '
        ...     'Each clay robot costs 2 ore. '
        ...     'Each obsidian robot costs 3 ore and 14 clay. '
        ...     'Each geode robot costs 2 ore and 7 obsidian.'
        ... )
        >>> bp = Blueprint(bp_text)
        >>> bp.list_affordable({'ore': 0, 'clay': 0, 'obsidian': 0})
        []
        >>> bp.list_affordable({'ore': 4, 'clay': 0, 'obsidian': 0})
        [RoboRecipe('ore', {'ore': 4}), RoboRecipe('clay', {'ore': 2})]
        >>> bp.list_affordable({'ore': 3, 'clay': 20, 'obsidian': 0})
        [RoboRecipe('clay', {'ore': 2}), RoboRecipe('obsidian', {'ore': 3, 'clay': 14})]
        """
        out = []
        for recipe in self.recipes:
            can_afford = all(
                recipe.cost[material] <= possessed
                for material, possessed in resources.items()
            )
            if can_afford:
                out.append(recipe)
        return out


class MiningProcess:
    def __init__(self, turns_left: int, robots=None, resources=None) -> None:
        self.turns_left = int(turns_left)
        if robots is None:
            self.robots = {'ore': 1, 'clay': 0, 'obsidian': 0, 'geode': 0}
        else:
            self.robots = robots
        if resources is None:
            self.resources = {'ore': 0, 'clay': 0, 'obsidian': 0, 'geode': 0}
        else:
            self.resources = resources

    def copy(self):
        return MiningProcess(
            turns_left=self.turns_left,
            robots=self.robots.copy(),
            resources=self.resources.copy()
        )

    def collect(self):
        for material, n_bots in self.robots.items():
            self.resources[material] += n_bots
        self.turns_left -= 1

    def build_robot(self, recipe: RoboRecipe):
        for material, cost in recipe.cost.items():
            if self.resources[material] < cost:
                raise ValueError(f'Not enough {material} to build {recipe.output} robot')
            self.resources[material] -= cost
        self.collect()
        self.robots[recipe.output] += 1

    def __repr__(self) -> str:
        return f"MiningProcess(turns_left={self.turns_left}, robots={repr(self.robots)}, resources={repr(self.resources)})"

    def can_focus_geodes(self, blueprint: Blueprint) -> bool:
        geode_bot = blueprint.get_recipe('geode')
        hypothetical = self.copy()
        while hypothetical.turns_left > 1:
            if not geode_bot.affordable(hypothetical.resources):
                return False
            hypothetical.build_robot(geode_bot)
        return True


def most_geodes(blueprint: Blueprint, state: MiningProcess) -> MiningProcess:
    """
    >>> bp_text_simple = (
    ...     'Blueprint 100: '
    ...     'Each ore robot costs 100 ore. '
    ...     'Each geode robot costs 20 ore.'
    ... )
    >>> bp_simple = Blueprint(bp_text_simple)
    >>> mp_simple = MiningProcess(turns_left=24)
    >>> most_geodes(bp_simple, mp_simple).resources['geode']
    3
    >>> bp_text1 = (
    ...     'Blueprint 1: '
    ...     'Each ore robot costs 4 ore. '
    ...     'Each clay robot costs 2 ore. '
    ...     'Each obsidian robot costs 3 ore and 14 clay. '
    ...     'Each geode robot costs 2 ore and 7 obsidian.'
    ... )
    >>> bp1 = Blueprint(bp_text1)
    >>> mp1 = MiningProcess(turns_left=24)
    >>> path1 = most_geodes(bp1, mp1)
    >>> path1.resources
    {'ore': 6, 'clay': 41, 'obsidian': 8, 'geode': 9}
    >>> bp_text2 = (
    ...     'Blueprint 2: '
    ...     'Each ore robot costs 2 ore. '
    ...     'Each clay robot costs 3 ore. '
    ...     'Each obsidian robot costs 3 ore and 8 clay. '
    ...     'Each geode robot costs 3 ore and 12 obsidian.'
    ... )
    >>> bp2 = Blueprint(bp_text2)
    >>> mp2 = MiningProcess(turns_left=24)
    >>> path2 = most_geodes(bp2, mp2)
    >>> path2.resources['geode']
    12
    """
    if state.turns_left == 0:
        return state
    if state.can_focus_geodes(blueprint):
        geode_bot = blueprint.get_recipe('geode')
        for _ in range(state.turns_left - 1):
            state.build_robot(geode_bot)
        state.collect()
        return state
    robo_choices = blueprint.list_affordable(state.resources)
    while not robo_choices:
        state.collect()
        if state.turns_left == 0:
            return state
        robo_choices = blueprint.list_affordable(state.resources)
    branches = []
    for recipe in robo_choices:
        choice = state.copy()
        choice.build_robot(recipe)
        branches.append(choice)
    no_build = state.copy()
    no_build.collect()
    branches.append(no_build)
    branch_maxes = [
        most_geodes(blueprint=blueprint, state=br)
        for br in branches
    ]
    return max(branch_maxes, key=lambda br: br.resources['geode'])


if __name__ == '__main__':
    import doctest
    doctest.testmod()
