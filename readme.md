# cl-reverse-read-file [![Build Status](https://travis-ci.org/charcodelimit/cl-reverse-read-file.svg?branch=readline-loop)](https://travis-ci.org/charcodelimit/cl-reverse-read-file)

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
      (loop :for line = (reverse-read-line str)
              :then (reverse-read-line str)
            :while line
            :collect line))))
```

```lisp
CL-USER> (reverse-file "test-1.dat")
("abc" "test" "3" "2" "1")
```

## Implementation - readline function

This implementation is based on a loop that reads chunks of the file
into memory starting from the current position of the file-stream
towards the start of the file. The last line without the newline
character following it is returned. The stream position is set to the 
last newline of the preceding line.

The implementation is efficient if the buffer size approximates the
average line length. For better understandability buffer creation and
skipping of the last newline character is separated from the
read-line loop.

## Usage

To load the [system definition](https://common-lisp.net/project/asdf/) and execute the tests run:
```lisp
(load "cl-reverse-read-file.asd")
(load "cl-reverse-read-file-test.asd")
(asdf:test-system :cl-reverse-read-file-test)
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
