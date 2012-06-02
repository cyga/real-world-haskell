real-world-haskell
==================

Parser for code samples from the book "real world haskell" (http://book.realworldhaskell.org/read/).

It's written (of course) in haskell.

Code samples aren't complete for different reasons. For example: sometimes parts of the file are given not in ascending order. The goal of this repo is to allow easy access / reduce copy/pasting for the code written in the book.

Some sources need fix in order to be compiled / work. In such cases original files saved under:
file.hs -> file.orig.hs

files
=====

  * parse-code.hs - download/parser of pages/program listings
  * \*.orig.hs - parsed program listings those need fix
  * else \*.hs - parsed program listings
