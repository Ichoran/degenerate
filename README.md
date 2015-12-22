# degenerate
Degenerate is an anti-code generator.  Write instances and degenerate will find a parent which generates them.

## Quick Start

Degenerate isn't written yet.  You can't actually get started.

## Principle of Operation

Degenerate assumes that the source code it will be reading is highly redundant, and that whitespace is not highly significant.  Furthermore, it tokenizes words according to typical source code rules, and has a special syntax to indicate where a break should occur (which can be placed inside a comment).

Then it forms n-grams for the token streams, keeping only those for which more than one instance is found.  It does this for small n-grams by just sliding a window over the token streams, and for large n-grams by extending off of the small n-grams.  These n-grams form a core of identity between files.  Note that this process assumes an underlying textual similarity; it will not recognize shape similarities such as the similarity between `(a+b+c)*d` and `(x-y-z)/w`.

(Todo: figure out how to lengthen these into blocks where there is merely similarity not identity.)
