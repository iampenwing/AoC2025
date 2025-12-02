# AoC2025

AT LAST! The Elves have a Project Manager!

Next up a Librarian/Archivist to help them get their documentation in order.

And, at some point, someone to fix their data structures and processes...

## Day 1

### Part 1

This was simple enough, count how many times we end on zero. Obviously parsing their bizzare input files is the oddest part of all this. 

### Part 2

ARGHHHHH... Why won't the logic work!?!?

As a result, this is a horrible bruite force method which manually clicks the dial and checks for zero on each click in the instruction set. 

This is wrong, all wrong...


## Day 2

### Part 1

So, there must be a way of getting the length of a number, right?

I couldn't find the right eBuild for the `Data.NumberLength` Haskell package.

`floor . logBase 10 x` *should* work, but in practice it doesn't quite - apparently the `logBase 10 1000` is not `3.0` but `2.9999999999999996` 

So I do ugly conversaions between `String` and `Int` a lot because I need to do increments etc.

### Part 2

This is, actually not much trickier... The main difference is iterating up through a list of possible ways of splitting each number, but you can avoid a significant number of checks by ignoring cases where the list won't split easily. In Part 1 I checked whether the input was an odd length, now I check whether the `mod` of the input length and the size of chunks is 0 or not...
