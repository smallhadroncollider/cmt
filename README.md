# cmt

Write consistent git commit messages

## Installation

```bash
stack build && stack install
```

## Usage

Add a `.cmt` file to your project directory.

### Format

A `.cmt` file consist of two parts: the input parts and the output format.

For example, the [AngularJS Commit Message Guidelines](https://gist.github.com/stephenparish/9941e89d80e2bc58a153):

```txt
{
    "Type" = [
        "feat",
        "fix",
        "docs",
        "style",
        "refactor",
        "test",
        "chore"
    ]
    "Scope" = @
    "Subject" = @
    "Body" = !@
    "Footer" = !@
}

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
