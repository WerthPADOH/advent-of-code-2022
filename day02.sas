/* https://adventofcode.com/2022/day/2 */

%macro run_game(
	choice_var1, choice_var2, score_var1, score_var2,
	rock_chars=AX, paper_chars=BY, scissors_chars=CZ
);
	%let rock = 1; %let paper = 2; %let scissors = 3;
	length _choice_value1 _choice_value2 8;
	%do ii = 1 %to 2;
		if find("&rock_chars.", &&choice_var&ii.) then _choice_value&ii. = &rock.;
		else if find("&paper_chars.", &&choice_var&ii.) then _choice_value&ii. = &paper.;
		else if find("&scissors_chars.", &&choice_var&ii.) then _choice_value&ii. = &scissors.;
    	&&score_var&ii.. = &&score_var&ii.. + _choice_value&ii.;
	%end;
	if _choice_value1 = _choice_value2 then do;
		&score_var1. = &score_var1. + 3;
		&score_var2. = &score_var2. + 3;
	end;
    else if _choice_value1 = &rock. and _choice_value2 = &scissors. then
		&score_var1. = &score_var1. + 6;
    else if _choice_value1 = &paper. and _choice_value2 = &rock. then
		&score_var1. = &score_var1. + 6;
    else if _choice_value1 = &scissors. and _choice_value2 = &paper. then
		&score_var1. = &score_var1. + 6;
    else if _choice_value1 = &rock. and _choice_value2 = &paper. then
		&score_var2. = &score_var2. + 6;
    else if _choice_value1 = &paper. and _choice_value2 = &scissors. then
		&score_var2. = &score_var2. + 6;
    else if _choice_value1 = &scissors. and _choice_value2 = &rock. then
		&score_var2. = &score_var2. + 6;
%mend run_game;

 /* Test on example data */
data test;
	length
		elf_choice my_choice $ 1
		elf_score my_score 8;
	retain elf_score my_score (0 0);
	input elf_choice $ 1 my_choice $ 3;
	%run_game(elf_choice, my_choice, elf_score, my_score);
	if _N_ = 3 and my_score ^= 15 then do;
		Put 'Failed test case, score was not 15';
		Put my_score=;
		stop;
	end;
	lines;
A Y
B X
C Z
;
run;

filename outfile 'day02-answers.txt';

data part1;
	file outfile;
	length
		elf_choice my_choice $ 1
		elf_score my_score 8;
	retain elf_score my_score (0 0);
	infile 'day02.txt' end = _last_obs;
	input elf_choice $ 1 my_choice $ 3;
	%run_game(elf_choice, my_choice, elf_score, my_score);
	if _last_obs then put my_score=;
run;

%let rock = 'A'; %let paper = 'B'; %let scissors = 'C';
%let lose = 'X'; %let tie = 'Y'; %let win = 'Z';

data part2;
	file outfile mod;
	length
		elf_choice my_choice outcome $ 1
		elf_score my_score 8;
	retain elf_score my_score (0 0);
	infile 'day02.txt' end = _last_obs;
	input elf_choice $ 1 outcome $ 3;
	if outcome = &tie. then my_choice = elf_choice;
	else if outcome = &lose. and elf_choice = &rock. then my_choice = &scissors.;
	else if outcome = &lose. and elf_choice = &paper. then my_choice = &rock.;
	else if outcome = &lose. and elf_choice = &scissors. then my_choice = &paper.;
	else if outcome = &win. and elf_choice = &rock. then my_choice = &paper.;
	else if outcome = &win. and elf_choice = &paper. then my_choice = &scissors.;
	else if outcome = &win. and elf_choice = &scissors. then my_choice = &rock.;
	%run_game(elf_choice, my_choice, elf_score, my_score);
	if _last_obs then put my_score=;
run;
