# cl-reverse-read-file [![Build Status](https://travis-ci.org/charcodelimit/cl-reverse-read-line.svg)](https://travis-ci.org/charcodelimit/cl-reverse-read-file)

The goal of this small programming exercise is to read a file 
from its end towards its start line by line in Common Lisp.

Additional requirements that should be fulfilled:
 - reading should be efficient
 - it should be possible to read just one line
 - it should be possible to read multiple lines
   by consecutively calling a function
 - the function should take a file-stream as argument

Example:

```
(defun reverse-file (pathname)
  "COLLECTs lines in file named PATHNAME starting from the end of the file"
  (with-open-file (str pathname
                       :direction :input
                       :element-type :default
                       :external-format :default)
    (let ((file-size (file-length str)))
      (file-position str file-size)
      ;;; read lines from stream and collect into a list while
      ;;; moving to the start of the file
      )))
```

```CL-USER> (reverse-file "test-1.dat")
("abc" "test" "3" "2" "1")```

## Usage

The master branch of this project contains a minimal set of files to get
started with an implementation for this problem:

 * the system definitions
 * a file for implementing a test suite
 * a file for implementing the algorithm
 * 5 test files covering different test-cases

The files with the test-data cover the following cases:

| filename | description |
|:-:|:-|
| test-1.dat | multiple lines, last line ends with newline |
| test-2.dat | multiple lines, last line without newline |
| test-3.dat | single-line, line ends with newline |
| test-4.dat | single-line, no newline |
| test-5.dat | empty file |

To load the [system definition](https://common-lisp.net/project/asdf/) and execute the tests run:
```(load "cl-reverse-read-file.asd")
(asdf:test-system :cl-reverse-read-file)```

For unit-testing [fiveam](https://github.com/sionescu/fiveam) is required, which can be loaded through e.g. [quicklisp](https://www.quicklisp.org) using:
```(ql:quickload "fiveam")```

## Implementations

This repository contains branches with different implementations for reading
lines from a file starting from its end.

| | branch name | description |
|:-:|:-|:-|
| [![Build Status](https://travis-ci.org/charcodelimit/cl-reverse-read-line.svg?branch=readline-loop)](https://travis-ci.org/charcodelimit/cl-reverse-read-file) | readline-loop | a readline function that is called in a loop, which returns NIL if no previous line is found |
| [![Build Status](https://travis-ci.org/charcodelimit/cl-reverse-read-line.svg?branch=callback-loop)](https://travis-ci.org/charcodelimit/cl-reverse-read-file) | callback-loop | a do-readline function that iterates through the file from last line to first line and executes a callback-function with the current line as argument |
| [![Build Status](https://travis-ci.org/charcodelimit/cl-reverse-read-line.svg?branch=do-readline-macro)](https://travis-ci.org/charcodelimit/cl-reverse-read-file) | do-readline-macro | a do-readline macro that iterates through the file from last line to first line and executes the loop body with the current line bound to a variable |

## License

cl-reverse-read-file is licensed under the [BSD](http://www.opensource.org/licenses/bsd-license.php) license.
