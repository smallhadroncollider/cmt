## Bugs

- Shouldn't be able to add blank Line

## Features

- List option?
    > Automatically adds a hyphen to each entry?
- Show options in flat list if short?
- Should search up directories to find .cmt
- Should support ~/.cmt for global option

## Done

- Format parser doesn't properly detect end of Literals
    > Should just stop on $ - might be a dollar sign that isn't a Named
- List item parsing too restrictive
    > List items can only contain letters and spaces
- ${*} name - from CLI argument
    > Wildcard name would accept whatever is passed in on command line: e.g. cmt "blah blah" - the "blah blah" goes in ${*} place
