# HBrainfuck

My toy implementation of the brainfuck programming language written in Haskell. It does _not_ aim to be as small as possible, being instead written to resresh my Haskell knowledge.

## Features

This project aims to be as close to the original brainfuck implementation (in terms of behaviour) as possible. Therefore, it has the following features:
* 8-bit unsigned integer cells with wrapping around (made with Haskell's `Word8` type);
* Infinite tape in both directions (instead of a unidirectional 30,000-cell tape in the origianl);
* The value 10 works as the newline code;
* When an end-of-file is reached upon execution of `,`, the value of the current cell is left unchanged.

Additional features and implementation details:
* Interactive mode (activated when the executable is run without a target file). Tape state persists between commands. An interactive session can be ended by inputting EOF;
* Comments are supported;
* Optimized cell increments and decrements, `[-]`, `[+]`;
* When there is a `]` in the code without preceding `[`, a parse error is raised.

## Running

The code from this project can be run with `cabal`:

    $ cabal run brainfuck

or by manually compiling it with GHC (the contents of `/src` must be in your current directory):

    $ ghc Main.hs

To actually run the compiled executable use

    > Main.exe <script>
    > Main.exe

on Windows, or

    $ ./Main <script>
    $ ./Main

on GNU/Linux.

## Status

The interpreter is fully functional.
