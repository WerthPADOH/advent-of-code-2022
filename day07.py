"""https://adventofcode.com/2022/day/7"""
from pathlib import Path

def process_file_system(lines: list) -> dict:
    """
    >>> fs = process_file_system(example_commands)
    >>> list(sorted(fs['directories'].keys()))
    ['/', '/a', '/a/e', '/d']
    >>> fs['directories']['/']
    48381165
    >>> fs['directories']['/a']
    94853
    >>> fs['directories']['/d']
    24933642
    >>> fs['directories']['/a/e']
    584
    """
    # {<path: str>: <size: int>}
    files = dict()
    directories = {'/': 0}
    lineage = Path('/')
    for line in lines:
        if line[:4] == '$ cd':
            dirname = line[5:]
            if dirname == '/':
                lineage = Path('/')
            elif dirname == '..':
                lineage = lineage.parent
            else:
                lineage = lineage.joinpath(dirname)
        elif line[:3] == 'dir':
            dirpath = lineage.joinpath(line[4:]).as_posix()
            if dirpath not in directories:
                directories[dirpath] = 0
        elif line[0].isdigit():
            size, _, name = line.partition(' ')
            size = int(size)
            filepath = lineage.joinpath(name)
            filepath_str = filepath.as_posix()
            if filepath_str not in files:
                files[filepath_str] = int(size)
                for ancestor in filepath.parents:
                    directories[ancestor.as_posix()] += size
    return {'files': files, 'directories': directories}


def sum_small_dirs(file_system, threshold) -> int:
    """
    >>> fs = process_file_system(example_commands)
    >>> sum_small_dirs(fs, 100000)
    95437
    """
    return sum(
        size for size in file_system['directories'].values()
        if size <= threshold
    )


def delete_dir_size(file_system, system_size=70000000, needed_space=30000000) -> int:
    """
    >>> fs = process_file_system(example_commands)
    >>> delete_dir_size(fs)
    24933642
    """
    used_size = file_system['directories']['/']
    free_space = system_size - used_size
    still_needed = needed_space - free_space
    delete_size = used_size
    for size in file_system['directories'].values():
        if still_needed <= size < delete_size:
            delete_size = size
    return delete_size


if __name__ == '__main__':
    import doctest
    test_context = globals().copy()
    test_context['example_commands'] = [
        '$ cd /',
        '$ ls',
        'dir a',
        '14848514 b.txt',
        '8504156 c.dat',
        'dir d',
        '$ cd a',
        '$ ls',
        'dir e',
        '29116 f',
        '2557 g',
        '62596 h.lst',
        '$ cd e',
        '$ ls',
        '584 i',
        '$ cd ..',
        '$ cd ..',
        '$ cd d',
        '$ ls',
        '4060174 j',
        '8033020 d.log',
        '5626152 d.ext',
        '7214296 k',
    ]
    doctest.testmod(globs=test_context)
    with open('day07.txt') as infile:
        commands = [line.strip() for line in infile]
    filesys = process_file_system(commands)
    # Part 1
    print(sum_small_dirs(filesys, 100000))
    # Part 2
    print(delete_dir_size(filesys))
