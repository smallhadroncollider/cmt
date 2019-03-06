# cmt

Write consistent git commit messages

## Installation

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
    "Scope" = @ # Allows a single line of input
    "Subject" = @
    "Body" = !@ # Allows multi-line input
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
- `["option 1", "option 2"]`: list of options

#### Output Format

The output format consists of named input parts plus anything else you want.

You can accept a output called `${*}`, which will add in whatever is passed to `cmt` as command line arguments.

For example:

```txt
# Input parts
# * input not needed, as comes from command-line
{
    "Scope" = @
}

# Scope from input and * from command-line
(${Scope}): ${*}
```

Then use with:

```bash
cmt "Blah blah blah"
```
