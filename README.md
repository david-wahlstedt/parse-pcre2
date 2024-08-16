# parse-pcre2

This is an implementation of a PCRE2 (v 10.45) parser, using
Text.ParserCombinators.ReadP.

It exports a function `parsePCRE` that produces an abstract syntax
tree given a correct PCRE2 pattern.

## Installation

### Install Haskell stack

See for instance
https://docs.haskellstack.org/en/stable/install_and_upgrade/

### Install parse-pcre2

```
git clone git@github.com:david-wahlstedt/parse-pcre2.git
cd parse-pcre2
make install
```

Running `make install` will run the code generation script in
[code_gen/](code_gen/), and then run `stack install`. The reason we
generate some code automatically is due to the long lists of character
properties, with names as well as their shortcuts. This is based on
the output of `pcre2test -LP` and `pcre2test -LS` (this only works for
PCRE2 v 10.44 or higher); these outputs are in the
[pcre2test/](pcre2test/) directory. If you want to generate property
names for a different version of PCRE2, issue those commands and
replace corresponding files' contents with the new outputs.

## Usage

There is a main program that can be called from the command-line, for
testing presupposes, that expects standard input. If the input has
several lines, these are considered as part of the pattern to parse.

Examples:
```
echo -n 'a|b.'|parse-pcre2
Alt [ Seq [ Lit 'a' ] , Seq [ Lit 'b' , Chartype CTAny ] ]
```
or from a file (we omit the output of space reasons)
```
parse-pcre2 < examples/date-1.txt
```
or for files with several lines, one regex each:
```
while IFS= read -r line || [ -n "$line" ]; do printf "%s" "$line" | parse-pcre2; done < examples/quoting.txt
```

## PCRE2 sample expressions

The examples in [examples/](examples/) are not yet systematic, but
they cover many kinds of PCRE2 patterns. Some of them are taken from
RFCs and YANG specifications, and some are hand-made for this parser.
Some examples should fail, but most of them should succeed.

## Limitations

- **Error handling**: There is no error handling: the parser just
  answers "No parse" if parse is unsuccessful.

- **Testing**: There are still no automatic tests. We have tested the
  examples in the aforementioned `examples` directory successfully,
  and more.

- **Performance**: Some parts of the parser are very inefficient, due
  to naive approaches. In particular, character types involving
  Unicode script names, like `\p{armi}`, etc. These are implemented
  with "loose matching", which means that a string is matched with the
  input after mapping to lowercase and removing whitespace, hyphens
  and underscore. It also involves linear search in a big table of
  possible script names. For instance, the following example (all the
  Unicode script names in one file) takes a long time to parse if
  taken as one big sequence, including the newlines::

  ```
  wc examples/char-type-all-scripts.txt
  322  322 3583 examples/char-type-all-scripts.txt

  time parse-pcre2 < examples/char-type-all-scripts.txt
  real	0m21,344s
  ```
  whereas
  ```
  time while read -r line || [ -n "$line" ]; do printf "%s" "$line" | parse-pcre2; done < examples/char-type-all-scripts.txt
  real	0m3,848s
  ```
  the same expressions are parsed much faster when separated into one command each.
  This needs to be investigated further.

## Some remarks on the code:

There is certainly room for improvement in the code, from a functional
programming point of view. The current version (as of 2024-08-16) does
not contain any innovations or particularly interesting parts, when it
comes to functional programming per se: the main goal was to have a
working parser, and that seems to be the case!

- **Infix operators**: The operators (mainly `<++`) coming with
  `ReadP` and using together with applicative operators (such as
  `<$>`, `<*>` and friends) is a bit unfortunate, since their
  precedences force the author to use overly many parentheses. Also,
  maybe using `<&>` (which is `flip <$>`) would be nicer, to get the
  parsing first and the transforming application after it.

- **Abstract syntax**: The current abstract syntax is somewhat a
  middle ground between two extremes: 1) retaining all information
  necessary to reconstruct the input string exactly, and 2) keeping
  the minimal amount of information necessary to match the same set of
  strings the PCRE2 expression matches. The reason for this is a bit
  arbitrary: it is the first that came into mind. We would like the
  constructors to be self documenting, but also not too long. Some of
  the constructors are just copies of the corresponding PCRE2
  counterparts, like Unicode script names, where others try to convey
  what they do, like `Many` for `*`, `Many1` for `+`, and `RepMin Int`
  for the `{,N}` quantifier. Some of the constructors could perhaps be
  better as records, for instance for character class or lookaround,
  that have different properties such as being positive/negative,
  forward/behind, etc.

- **Higher level functional programming**: Not so much. But when we
  introduce taking option settings into account for the parsing, we
  will probably need monad transformers, and this will call for more
  elegant solutions, not to blow up the code complexity too much.

- **Module headers**: There are no export lists, and a lot of
  suppressed warnings. This is for convenience, and should be changed.

## Related work

The Haskell
[Regular_expressions](https://wiki.haskell.org/Regular_expressions)
page has an overview of regex support, and some C bindings to PCRE.
However, we haven't found any parser covering the whole PCRE2
language. We may try to connect this parser with some of the existing
libraries in the future.
