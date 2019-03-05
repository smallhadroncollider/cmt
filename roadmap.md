## Bugs

- pre-commit errors don't get displayed at all
    > Probably need to show stderr in some cases?

## Features

- Store previous commit info if it fails
    > Store if commit fails. `cmt --prev` option?
- List option?
    > Automatically adds a hyphen to each entry?
- Add comments to .cmt
- Show options in flat list if short?
- Make parts optional
    > @? and !@? operators?
- Option to show files that have changed
    > Useful for things like ${Scope} - autocomplete maybe?

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
