# Sedlying - `sedlex` buffers for marking at boundary codepoints

Sedlex buffers which ignore mark requests at non-boundary codepoints. This means
they lie about exactly adhering to the interface required by `sedlex`.

This needs a version of `sedlex` which allows for marks to be ignored. In
particular, if there are no transitions out of a DFA state, `sedlex` should mark
with a value and immediately backtrack instead of returning the value. You can
find an appropriate fork of `sedlex` in the `mark-ignore` branch of [this
repo](https://github.com/ifazk/sedlex/tree/mark-ignore).

# Installing
You can install the package with:

    opam pin add sedlying https://github.com/ifazk/sedlying.git

# Documentation
The API documentation can be found [here](https://ifazk.github.io/sedlying/).

## LICENSE
Written in 2019 by Ifaz Kabir

This project uses the MIT license. `LICENSE.txt` for details.
