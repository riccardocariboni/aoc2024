# aoc2024

Advent of Code 2024 problems in Common Lisp (SBCL).

## Usage

Note: The cl-ppcre library can be installed running `(ql:quickload :cl-ppcre)`

FIrst place the system in a place where `asdf` can find it, then load the system in a REPL (SLIME is suggested) by running `(asdf:load-system :aoc2024)`.

After the system has been loaded, you can call the `solve-p1` and `solve-p2` functions for day XX by running `(aoc2024-XX:solve-pY "path/to/the/input/file")`.

## License

GPL v3

