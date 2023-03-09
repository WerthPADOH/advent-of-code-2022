"""https://adventofcode.com/2022/day/25"""
import math


def int_to_snafu(x: int) -> str:
    x = int(x)
    high_power = math.ceil(math.log(x, 5))
    r = x
    digits = [None] * (high_power + 1)
    for exp in range(high_power, -1, -1):
        q, r = divmod(r, 5**exp)
        digits[exp] = q
    for ii, val in enumerate(digits):
        if val is None:
            digits[ii] = '0'
            continue
        if val >= 5:
            carryover, val = divmod(val, 5)
            digits[ii + 1] += carryover
        if 0 <= val <= 2:
            digits[ii] = str(val)
        elif val == 3:
            digits[ii + 1] += 1
            digits[ii] = '='
        elif val == 4:
            digits[ii + 1] += 1
            digits[ii] = '-'
    while digits[-1] == '0':
        digits.pop()
    return ''.join(reversed(digits))


def snafu_to_int(x: str) -> int:
    multipliers = {'0': 0, '1': 1, '2': 2, '-': -1, '=': -2}
    return sum(
        multipliers[char] * 5**exp
        for exp, char in enumerate(reversed(x))
    )


if __name__ == '__main__':
    test_cases = [
        (1, '1'),
        (2, '2'),
        (3, '1='),
        (4, '1-'),
        (5, '10'),
        (6, '11'),
        (7, '12'),
        (8, '2='),
        (9, '2-'),
        (10, '20'),
        (15, '1=0'),
        (20, '1-0'),
        (2022, '1=11-2'),
        (12345, '1-0---0'),
        (314159265, '1121-1110-1=0'),
    ]
    for decimal, snafu in test_cases:
        made_int = snafu_to_int(snafu)
        made_snafu = int_to_snafu(decimal)
        if made_snafu != snafu:
            raise RuntimeError(f"int_to_snafu({decimal}): expected '{snafu}', got '{made_snafu}'")
        if snafu_to_int(snafu) != decimal:
            raise RuntimeError(f"snafu_to_int('{snafu}'): expected {decimal}, got {made_int}")
    example = [
        '1=-0-2', '12111', '2=0=', '21', '2=01', '111', '20012',
        '112', '1=-1=', '1-12', '12', '1=', '122',
    ]
    assert int_to_snafu(sum(snafu_to_int(ex) for ex in example)) == '2=-1=0'

    with open('day25.txt') as infile:
        snafus = [line.strip() for line in infile]

    # Part 1
    snafu_sum = sum(snafu_to_int(ss) for ss in snafus)
    print(int_to_snafu(snafu_sum))
