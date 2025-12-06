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


## Day 3

### Part 1

I'm sure I could have done this part much smoother by using a couple of `max` functions rather than my more manual recursive method, but meh... who cares...

### Part 2

Oh. 

Complete refactor of the code. Well... not completely, just enough... I needed to keep better track of postioions of things though so maybe `max` functions wouldn't have worked.

Took me a bit to get the counting logic right in finding the highest and it's position - I thought I could track position int he tuple... why am I such an idiot?

## Day 4

### Part 1

Why do I not have a `countAround` function yet... oh yeah, because it's all too specific every puzzle...

### Part 2

OK, let's reverse the polarity of some of the conditionals and add some iterative... and blam


## Day 5

I was sure this was going to be best done in `Set`s for both parts, but no... everything is too big. Better to let it all just stay as Ranges and calculate based on them

## Day 6

Oooh... was expecting something harder here. This is just about getting the list of numbers parsed properly and bam.

Part 2 wasn't even much harder when you recognise you can just rotate the grid and bam!
