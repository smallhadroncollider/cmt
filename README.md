# cmt

Write consistent git commit messages

## Install

[Binaries for Mac and Linux are available](https://github.com/smallhadroncollider/cmt/releases). Add the binary to a directory in your path (such as `/usr/local/bin`).

### Cabal

**Requirements**: [Cabal](https://www.haskell.org/cabal/)

```bash
cabal install cmt
```

Make sure you run `cabal update` if you haven't run it recently.

### Building

**Requirements**: [Stack](https://docs.haskellstack.org/en/stable/README/)

The following command will build cmt and then install it in `~/.local/bin`:

```bash
stack build && stack install
```


## Usage

Add a `.cmt` file to your project directory.

```bash
cmt # will show the options and then commit
```

If you're using the `${*}` format option then:

```bash
cmt "blah blah blah" # this will go in ${*} place
```

### Format

A `.cmt` file consist of two parts: the input parts and the output format.

For example, the [AngularJS Commit Message Guidelines](https://gist.github.com/stephenparish/9941e89d80e2bc58a153):

```txt
# The input parts
{
    # Shows a list of options
    "Type" = [
        "feat",
        "fix",
        "docs",
        "style",
        "refactor",
        "test",
        "chore"
    ]
    "Scope" = % # Select from a list of staged files
    "Subject" = @ # Single line input
    "Body" = !@ # Multi-line input
    "Footer" = !@
}

# The output format
# Takes the values provided from the input stage
# and interpolates them in
${Type} (${Scope}): ${Subject}

${Body}

${Footer}
```


#### Input Parts

These are at the top of the `.cmt` file and surrounded by opening and closing curly braces. A consist of a name and a type:

- `@`: single line input
- `!@`: multi line input
- `%`: select from a list of staged files
- `["option 1", "option 2"]`: list of options

#### Output Format

The output format consists of named input parts plus anything else you want.

You can accept a output called `${*}`, which will add in whatever is passed to `cmt` as command line arguments.

For example:

```txt
# Input parts
# * input not needed, as comes from command-line
{
    "Scope" = %
}

# Scope from input and * from command-line
(${Scope}): ${*}
```

Then use with:

```bash
cmt "Blah blah blah"
```
