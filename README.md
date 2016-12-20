# SchemeInterpreter


[![Build Status](https://travis-ci.org/demonyangyue/SchemeInterpreter.svg?branch=master)](https://travis-ci.org/demonyangyue/SchemeInterpreter)

My scheme interpreter written in scala, to get a better understanding of the book [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala), especially for the chapter 9 - Parser Combinators

## Installation

```bash
git clone https://github.com/demonyangyue/SchemeInterpreter
```

## Execution

### Run Interactively 

```
cd SchemeInterpreter
sbt run

//the programme will pause and wait for user input

> (+ 1 (+ 1 1))
3

> (define (fact x) (if (= x 0) 1 (* x (fact (- x 1)))))
null
> (fact 10)
3628800

```
### Get Input from File

You can also put a collection of Scheme statements in a file, then evaluating them in a batch, for example:

```bash
sbt run < src/main/resources/input_file
```
## Test

```bash
sbt test
```
## Further Improvement

* Cover all the scheme interpreter statements
* More comprehensive error reporting

