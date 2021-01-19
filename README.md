# Sudoku Solver

## Overview

This repo contains two reimplementations of [Peter Norvig's Sudoku Solver](https://norvig.com/sudoku.html). There is a mostly direct translation in the `sudoku.lisp` file in the src folder. There is also a highly optimized reimplementation in `sudoku-fast.lisp` that deviates pretty far from Norvig's implementation.

## Direct Translation

The direct translation follows the same basic algorithms, but I did change the data structures to try and make it more natural (and hopefully more performant) under lisp. Instead of the hashtable for storing the data, I used a 9x9 vector. Instead of storing the remaining numbers in a string, I used a list of numbers. My initial version was slower than the python version, so I added type information and optimization hints to get it roughly equivalent to the numbers cited in the original essay.

I was surprised that it was hard to get the implementation to parity with the cited numbers. Even more surprising, when I downloaded the python script and ran it on my machine, the python code was even zippier. Meaning, my "optimized" direct translation code is still slower when running on my machine than is the python code. I'm guessing that python dictionaries and strings are pretty well optimized. Plus the lazy enumerations/generators mean that the code shouldn't ever be computing things it's not going to need.

At some point I may do a 100% direct translation using hashtables and strings. I didn't the first go around because strings and hashtables are much clunkier in common lisp than in python. My impression is that they are also slower, but I would need to write the code and time things.

## Faster Translation

When running the direct translation code in the profiler I hit a wall with lisp's `member` function. I was able to improve things somewhat with my custom `member-p` function, but that too is also the bottleneck (though not _as_ much). The only way I could think of to improve that is to change how the data is represented so that membership tests are O(1).

The main idea is to store remaining members as bits in a fixnum. To really see if I could reduce bottlenecks I switched to an 81 element single dimension array in an attempt to stop [sbcl](http://www.sbcl.org/) from complaining about slow vector access. I also added type information everywhere and turned all of the optimization settings [up to 11](https://www.youtube.com/watch?v=4xgx4k83zzc) for most of the functions. The outline of the algorithm is the same, but all of the details have changed. Basically, the code now looks like C code with too many parenthesis and not enough brackets.

The code powers through the easy and hard problems quickly. But so does the python code. In fact they are both so fast on these easier problems that trying to measure the differences would require finesse and careful measurements. I'm not going to bother. However, the hardest sudoku listed on the the sudoku page still takes a while for the python code to solve. On my machine the python code solves `.....6....59.....82....8....45........3........6..3.54...325..6..................` in 57.5 seconds. The faster translation done in Common Lisp solves it in 3.4 seconds.
