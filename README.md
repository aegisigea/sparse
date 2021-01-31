# Sparse Matrix Libary in Futhark

Welcome to our group project for the course Parallel Functional Programming
([NDAK14009U][1]), which involved designing and implementing a library of sparse
matrix operations in Futhark.

[1]: https://kurser.ku.dk/course/ndak14009u/2020-2021

## System Guide

- `app/`: Contains a few example applications for the library.
- `src/`: Contains the source code for the library.
- `tst/`: Contains the tests and benchmarks of the library.

## User Guide

- `make deps`: Downloads the necessary Futhark and Python dependencies.
- `make test`: Executes the suite of unit and property-based tests.
- `make bench`: Evaluates the benchmarks and writes the result to JSON.
- `make graph`: Generates the figures visualizing the evaluated benchmarks.
- `make clean`: Cleans the generated dependencies, auxiliaries and executables.
