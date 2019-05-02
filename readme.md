# cl-reverse-read-file [![Build Status](https://travis-ci.org/charcodelimit/cl-reverse-read-file.svg?branch=do-readline-macro)](https://travis-ci.org/charcodelimit/cl-reverse-read-file)

The goal of this small programming exercise is to read a file 
from its end towards its start line by line in Common Lisp.

Additional requirements that should be fulfilled:
 - reading should be efficient
 - it should be possible to read just one line
 - it should be possible to read multiple lines
   by consecutively calling a function
 - the function should take a file-stream as argument

Example:

```lisp
(defun reverse-file (pathname)
  "COLLECTs lines in file named PATHNAME starting from the end of the file"
  (with-open-file (str pathname
                       :direction :input
                       :element-type :default
                       :external-format :default)
    (let ((file-size (file-length str)))
      (file-position str file-size)
      (do-reverse-read-line (line str)
        (push line lines))
      (nreverse lines))))
```

```lisp
CL-USER> (reverse-file "test-1.dat")
("abc" "test" "3" "2" "1")
```

## Implementation - do-readline macro

The implementation is based on a macro that generates a loop
whose body is executed with the current line bound to a variable.

A buffer is used to read the file block-wise into memory starting
from the current position in the file-stream towards the 
start of the file. The stream-position is adjusted such that 
it is possible to read preceding lines by repeated calls to
the do-readline macro.

Due to the implicit block named `NIL` in loop macros only the outer
loop was implemented using the loop macro. This allows *return*ing 
from the outer loop in the do-readline macro's body:
```lisp
(do-reverse-read-line (line str)
  (when (equal "goal" line) (return T)))
```

## Usage

To load the [system definition](https://common-lisp.net/project/asdf/) and execute the tests run:
```lisp
(load "cl-reverse-read-file.asd")
(asdf:test-system :cl-reverse-read-file)
```

For unit-testing [fiveam](https://github.com/sionescu/fiveam) is required, which can be loaded through e.g. [quicklisp](https://www.quicklisp.org) using:
```lisp
(ql:quickload "fiveam")
```

The files with the test-data cover the following cases:

| filename | description |
|:-:|:-|
| test-1.dat | multiple lines, last line ends with newline |
| test-2.dat | multiple lines, last line without newline |
| test-3.dat | single-line, line ends with newline |
| test-4.dat | single-line, no newline |
| test-5.dat | empty file |

## License

cl-reverse-read-file is licensed under the [BSD](http://www.opensource.org/licenses/bsd-license.php) license.
