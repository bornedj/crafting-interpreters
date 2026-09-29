# Crafting interpreters

Following the crafting interprets book by Robert Nystrom.

I'm expecting to at least deliver the tree walking interpreter with haskell. I
may build the compiler with it as well, but could also switch to rust at that
point. The goal is to further my understanding of compilers before working on
an IR for the schemactl project.

## Chapter 4 (Scanning) - remaining work

High-level steps left to consider chapter 4 done, roughly in the order they
unblock each other:

1. Finish the literal parsers (`identifier`, `number`, `string`) in
   `literalToken`. Have `identifier` look its matched text up in a keyword
   table, falling back to `Literal Identifier`, replacing the current
   keyword-alternation approach so keywords no longer break on maximal munch
   (e.g. `andy`).
2. Add a `//` line comment parser, tried before `singleCharToken` so it is not
   shadowed by the single `/` case.
3. Decide the position-tracking questions (offset field for O(1) lexeme
   slicing, 0- vs 1-indexed columns), then write the `located` combinator that
   turns a `TokenType` parser into a `Token` parser.
4. Assemble `scanTokens` from `many` and `eof`, appending a trailing `Eof`
   token the way the book's scanner does.
5. Add error recovery so a bad character is reported and scanning continues,
   rather than stopping at the first bad character.
6. Flesh out `ErrorHandler` (`report`, `error`, a `hadError` flag) and wire
   exit codes: 64 for usage errors, 65 for a scan error.
7. Wire `run` in `exe/Main.hs` to the lexer, and fix the REPL loop (end of
   input handling, blank-line behaviour).
8. Close the test gaps: `singleCharToken`, newline/column tracking across
   multi-line input, and a multi-line string literal.
