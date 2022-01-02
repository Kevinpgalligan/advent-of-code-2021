### Description
Advent of Code 2021 solutions in Common Lisp.

There's no framework for running the solutions, you just have to compile & load the .lisp file (I use emacs + SLIME for this purpose), then use whatever input-parsing function is provided to load the problem input, then pass that input to whichever function is provided to solve the problem! Might take some figuring out. Also, not all days have input-parsing functions because the input is so simple (e.g. Day 17).

#### Plotting linecounts
Run this to plot the line counts of all the solutions.

    find . -name '*.lisp' | xargs wc -l | grep -v total | awk '{ print $2 " " $1 }' | sort | awk 'BEGIN { day=1 } { print day " " $2; day += 1; }' | python3 aocplot.py
