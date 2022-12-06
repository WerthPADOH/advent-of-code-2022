/* https://adventofcode.com/2022/day/3 */

function find_repeat(sack) {
    compartment1 = sack.substring(0, sack.length / 2);
    compartment2 = sack.substring(sack.length / 2);
    for (char of compartment1) {
        if (compartment2.includes(char)) {
            return char
        }
    }
}


function score_priority(char) {
    ordering = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    return ordering.indexOf(char) + 1;
}


function sum_priorities(sacks) {
    total = 0;
    for (sack of sacks) {
        repeated = find_repeat(sack);
        priority = score_priority(repeated);
        total = total + priority;
    }
    return total
}


async function main() {
    /* Test with example */
    example = [
        'vJrwpWtwJgWrhcsFMMfFFhFp',
        'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL',
        'PmmdzqPrVvPwwTWBwg',
        'wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn',
        'ttgJtRGJQctTZtZT',
        'CrZsJsPPZsGzwwsLwLmpwMDw'
    ];
    example_res = sum_priorities(example);
    if (example_res != 157) {
        console.log('Expected 157, got ' + example_res);
        throw new Error('Failed the example!');
    }
    const infile = new File([], 'day03.txt', {type: 'text/plain'});
    reader = new FileReader();
    reader.readAsText(infile);
    const intext = reader.result;
    rucksacks = intext.result.split('\n');
    result1 = sum_priorities(rucksacks);
    console.log(result1);
}


main()
