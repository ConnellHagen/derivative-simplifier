# Derivative Simplifier

## About
This is an OCaml program that takes a simple derivative expression, and simplifies it as much as possible using a set of derivative rules. Each step will then be laid out in a proof-like format to show each step.

![Example of d/dx 3x/4](https://user-images.githubusercontent.com/72321241/235239018-d78847f8-a657-4da9-a10f-fddcde409ad9.png)

## Usage
Step 1: run `dune build` to build the executable file

Step 2: run `_build/default/bin/main.exe data/rules.ddx -e "<your expression here>"`

### Notes for Running
- All operations should be explicitly stated, ex. 3x -> 3*x
- A derivative is represented by `d/dx <expr>`
- Functions like `cos` may be included in your expression, but they will not be simplified
- You may add your own rules by adding them to `rules.ddx`, but they may not work as intended if they are too different from the 20 that were kept in mind when this program was wrote.
- Integer math is not simplified, but the answers are still correct.
