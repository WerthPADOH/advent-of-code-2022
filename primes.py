import collections
import math


def _prime_sieve(max):
    max = int(max)
    numbers = range(max + 1)
    factored = [False] * len(numbers)
    factored[0] = True
    factored[1] = True
    prime_cutoff = math.ceil(math.sqrt(max))
    for num in numbers:
        if not factored[num]:
            yield num
            if num <= prime_cutoff:
                for ii in range(2 * num, max + 1, num):
                    factored[ii] = True


def prime_generator(max=None):
    """
    >>> g = prime_generator(max=10)
    >>> next(g)
    2
    >>> next(g)
    3
    >>> list(g)
    [5, 7]
    >>> g = prime_generator()
    >>> next(g)
    2
    >>> next(g)
    3
    >>> [next(g) for _ in range(10)]
    [5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
    """
    # More efficient to use a sieve if known max
    if max is not None:
        yield from _prime_sieve(max)
    else:
        yield 2
        primes = set()
        ii = 3
        while True:
            if not any((ii % p) == 0 for p in primes):
                yield ii
                primes.add(ii)
            ii += 2


def factors(num: int):
    """
    >>> factors(2)
    {2: 1}
    >>> factors(8)
    {2: 3}
    >>> factors(63)
    {3: 2, 7: 1}
    """
    num = int(num)
    fac = collections.defaultdict(int)
    max_factor = math.ceil(math.sqrt(num))
    for p in prime_generator(max=max_factor):
        if p > num:
            break
        while True:
            quotient, remainder = divmod(num, p)
            if remainder == 0:
                fac[p] += 1
                num = quotient
            else:
                break
    if len(fac) == 0:
        return {num: 1}
    return dict(fac)


def lcm(*args):
    """
    >>> lcm(9, 12)
    36
    >>> lcm(12, 9)
    36
    >>> lcm(27, 1)
    27
    >>> lcm(27, 1, 0)
    0
    >>> lcm(54, 54, 54)
    54
    """
    max_factors = dict()
    for x in args:
        x = int(x)
        if x == 0:
            return 0
        for prime, exponent in factors(x).items():
            current_exp = max_factors.get(prime, 0)
            if exponent > current_exp:
                max_factors[prime] = exponent
    out = 1
    for pp, ee in max_factors.items():
        out *= pp**ee
    return out
