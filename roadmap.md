## Bugs

- Multi-line is always optional
- Should throw an error if no staged changes?

## Features

- List option?
    > Automatically adds a hyphen to each entry?
- Make parts optional
    > @? and !@? operators?
- Character limits
- Line length limits
- Descriptions alongside part name
- Repeated sections
    > e.g. Co-Author: ${Name} ${Email}
- Optional output bits
    > e.g. (${Scope}) shouldn't show brackets if no Scope provided

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
