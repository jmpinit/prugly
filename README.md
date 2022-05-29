# prugly

Assembler, disassembler, and debugger for the [Programmable Realtime
Units](http://hackaday.com/2014/06/22/an-introduction-to-the-beaglebone-pru/) in the [BeagleBone single
board computers](https://beagleboard.org/bone) written in Racket.

Racket is designed for language-oriented programming. This project is an experiment in exploring using Racket to implement a low-level programming toolkit.

The primary goal is to be able to write code like the following:

```
#lang prugly

ldi  r4, 4919
ldi  r31.b0, 35
halt
```

And have it be assembled into a binary that can run on the PRU when evaluated by Racket.

## Running the Example

```bash
racket examples/load-halt.prugly
```

## Tests

```bash
racket tests/test-assembler.rkt
```

