# Sedlying - `sedlex` buffers for marking at boundary codepoints

Sedlex buffers which ignore mark requests at non-boundary codepoints. This means
they lie about exactly adhering to the interface required by `sedlex`.

This needs a version of `sedlex` which allows for marks to be ignored. In
particular, if there are no transitions out of a DFA state, `sedlex` should mark
with a value and immediately backtrack instead of returning the value. You can
find an appropriate fork of `sedlex` in the `mark-ignore` branch of [this
repo](https://github.com/ifazk/sedlex/tree/mark-ignore).

## Use with Care
These buffers will only prevent markings from being performed at non-boundary
points, the `sedlex` regular expressions still work at the codepoint level. So
even with an input with boundaries only at grapheme clusters, `"g\u{308}"` will
match the regular expression `"g", any` even though users generally percieve
`"g\u{308}"` to be a single character.

# Installing
You can install the package with:

    opam pin add sedlying https://github.com/ifazk/sedlying.git

## Development
To run tests, you are going to need `uugen` and a patched version of `sedlex`.

    opam pin add sedlex https://github.com/ifazk/sedlex.git#mark-ignore
    opam pin add uugen https://github.com/ifazk/uugen.git

# Documentation
The API documentation can be found [here](https://ifazk.github.io/sedlying/).

## LICENSE
Written in 2019 by Ifaz Kabir

This project uses the MIT license. `LICENSE.txt` for details.

## Acknowledgements
Some parts of the source code are adapted from `sedlex`.
