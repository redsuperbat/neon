# Neon

<!--toc:start-->

- [Neon](#neon)
  - [Features](#features)
  - [Todo](#todo)
  <!--toc:end-->

Expression based language written in rust 🦀
I am working on this language in my free time. This is a passion project and wont by most means lead to anything.
The aim is to make a 100% async runtime without the need to mark functions/methods as async.
All functions are async, nothing is sync. This will be done in order to avoid the red/blue function problem

## Features

- Higher order functions
- Math operators
- If statements
- Recursion
- Immutable variables
- Type Checker

Feel free to try it out in the [playground](https://redsuperbat.github.io/neon/)

Everything is made from scratch, the neon_core library has no dependencies and the syntax highlighting is custom made by interpreting the tokens and adding decorations to the monaco editor.

![redsuperbat github io_neon_](https://github.com/user-attachments/assets/74e56300-4173-4156-a7bc-ec31d023d8ad)

## Todo

- [ ] Mathematical operator precedence
- [ ] Module system
- [ ] Lua compiler
- [ ] Structs and methods
- [ ] Async runtime
- [ ] IO std lib
- [ ] String interpolation
- [ ] Type syntax for functions
