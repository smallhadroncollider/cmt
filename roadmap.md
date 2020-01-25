## Bugs

- Multi-line is always optional
- Multi-line always shows two entry inputs, even if first is empty
- Should throw an error if no staged changes?
- Should throw an error if `.cmt.bkp` missing and using `--prev`

## Features

- Character limits
- Line length limits
- Make parts optional
    > @? and !@? operators?
- List option?
    > Automatically adds a hyphen to each entry?
- Repeated sections
    > e.g. Co-Author: ${Name} ${Email}
- Optional output bits
    > e.g. (${Scope}) shouldn't show brackets if no Scope provided
- Descriptions alongside part name
- Free-text option in option lists

## Doing


## Done

- Format parser doesn't properly detect end of Literals
    > Should just stop on $ - might be a dollar sign that isn't a Named
- List item parsing too restrictive
    > List items can only contain letters and spaces
- ${*} name - from CLI argument
    > Wildcard name would accept whatever is passed in on command line: e.g. cmt "blah blah" - the "blah blah" goes in ${*} place
- Shouldn't be able to add blank Line
- Better error message for * missing
    > If ${*} detected, then say if no command line argument given
- Should search up directories to find .cmt
- Should support ~/.cmt for global option
- Add comments to .cmt
- pre-commit errors don't get displayed at all
    > Probably need to show stderr in some cases?
- Show options in flat list if short?
- Option to show files that have changed
    > Useful for things like ${Scope} - autocomplete maybe? `git diff --name-only`
- Need to escape shell characters in Cmt.IO.Git
- Store previous commit info if it fails
    > Store if commit fails. `cmt --prev` option?
- Extra `\n` at end of output
- Should strip empty space/newlines from end of file
- Way to store pre-written commits
    > Occasionally used messages: e.g. "version bump", "latest notes", etc.
- Should pipe input out as it's happening
- XDG Base Directory support for `.cmt` file
- `--dry-run` option
