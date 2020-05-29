# AI class snippets

This repo is just a piece of my praxis during the Introduction to AI class in my university. The two lisp files in this repo are:
- A solution for the [Eight Queens](https://en.wikipedia.org/wiki/Eight_queens_puzzle) puzzle with the [genetic algorithm](https://en.wikipedia.org/wiki/Genetic_algorithm)
- A solution for the [Bridge and Torch problem](https://en.wikipedia.org/wiki/Bridge_and_torch_problem) with the help of [Simulated Annealing](https://en.wikipedia.org/wiki/Simulated_annealing) (in a simplest variation possible)

## Eight Queens / Genetic algo

This one is an fairly optimized genetic algorithm solution, based on the vectors (1D arrays). Every generation is a pack of vectors with the contents that arise from breeding of the two most succesful vectors of the previous generation. I guess having a bit more randomness (like a small possibility of non-successful vectors reproduction to escape local maxima) would be nice, but it's already good as it is.

Given all these words about the optimization, you'd expect it to run mementarily. Oh how mistaken you are there... It takes from 0.2 to 2 seconds (approximately) for different runs. The reason, I guess, is the inoptimal breeding, but I already have neither time nor interest in perfecting the old learning code snippets -- sometimes you need to let these things go. Here's the measurements for 100 runs of this:

``` shell
CL-USER> (time (loop repeat 100 do (genetic-queens 8 100)))
Evaluation took:
  96.861 seconds of real time
  96.922912 seconds of total run time (96.911407 user, 0.011505 system)
  [ Run times consist of 0.288 seconds GC time, and 96.635 seconds non-GC time. ]
  100.06% CPU
  213,969,206,656 processor cycles
  7,510,866,928 bytes consed
```

**To run** it compile the whole file (or buffer, *if you know what I mean*) and run:

``` lisp
(genetic-queens 8 100)
```

First argument is the width (and heigth) of a square chess board. And the second argumant is how much separate positions will initially be generated for selection.

## Bridge and Torch / Simulated Annealing

Another classical problem and another classical and more obscure solution. An extremely simple function is used for the Simulated Annealing choice, the random number-based one, which is not even a typical simulated annealing function, but it was sufficient for the test runs. Yes, I'd use the exponentially declining function for a more serious case, but c'mon, it's a quick code snippet to make hands dirty. Remember, you need to let things go sometimes.

Here's how you can make a test run. First you need to define \*WALK-TIMES\* as a new property list (the one where there is sequence of keyword-value pairs) and to call BRIDGE-GEN with RANDOM-RESTART-WRAPPER around it. The keywords, representing the people on the bridge, should be the same as the keywords in the \*WALK-TIMES\*.

``` lisp
(let ((*walk-times* '(:a 1 :b 2 :c 5 :d 10 :e 20)))
  (random-restart-wrapper 100 #'minimal-time-function (bridge-gen '(:a :b :c :d :e))))
```
