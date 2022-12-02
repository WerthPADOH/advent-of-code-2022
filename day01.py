"""https://adventofcode.com/2022/day/1"""

def parse_loads(stream):
    """
    >>> intext = [
    ... '1000', '2000', '3000',
    ... '',
    ... '4000',
    ... '',
    ... '5000', '6000',
    ... '',
    ... '7000', '8000', '9000',
    ... '',
    ... '10000',
    ... ]
    >>> parse_loads(intext)
    [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]
    """
    loads = []
    current = []
    for line in stream:
        line = line.strip()
        if line == '':
            loads.append(current)
            current = []
        else:
            current.append(int(line))
    if current:
        loads.append(current)
    return loads


if __name__ == '__main__':
    import doctest
    doctest.testmod()

    with open('day01.txt') as infile:
        elf_loads = parse_loads(infile)
    total_loads = [sum(load) for load in elf_loads]
    max_load = max(total_loads)
    print(max_load)

    total_loads.sort()
    print(sum(total_loads[-3:]))
