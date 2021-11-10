# Parser Project from CS-441


## Introduction
Hello and welcome to my parser project that I created for my Programming Language: Design & Implementation class! The goal of this project was to create a parser, which is the second step in the compliation process (right after the lexical analysis, AKA scanner) for a simple calculator grammar. This parser is designed to return three different outputs when given a program in the calculator language. The first is that the program parses correctly (and thus the code follows the syntax rules of the grammar), the second being a parse error and the line that it is on, and the third being a scanner error and the line that it is on as well. This parser is a LL(1) top down parser which is implemented from a FIRST, FOLLOW, and PREDICT set. The scanner was also created in order to pass the tokens to the parser, and that is in a separate file named "scanner.rkt".

## How to run
You will need [DrRacket](https://racket-lang.org/), a free interpreter for the Racket language (which is the langauge this project was written in), in order to run this. Once the parser.rkt has been launched successfully with DrRacket, you will hit the run button, which will bring up a console at the bottom. You will need to call the parse function manually for each file in order to run. Below are the function calls with the correct syntax that are needed to call all of the sample input files provided.

```Racket
(parse "input/Input01.txt")
```

```Racket
(parse "input/Input02.txt")
```

```Racket
(parse "input/Input03.txt")
```

```Racket
(parse "input/Input04.txt")
```

```Racket
(parse "input/Input05.txt")
```

```Racket
(parse "input/Input06.txt")
```
