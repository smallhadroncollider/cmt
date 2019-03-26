# Contributing

## Bug Reports/Feature Requests

Please check the [roadmap.md](https://github.com/smallhadroncollider/cmt/blob/develop/roadmap.md) before adding any bugs/feature requests to Issues.

## Code

*Please use the `develop` branch as the base for any pull requests*

Anyone is welcome to contribute to the project. Check out [roadmap.md](https://github.com/smallhadroncollider/cmt/blob/develop/roadmap.md) for bugs and planned features if you're not sure where to start.

If you're planning on doing a big chunk of work it's probably best to [contact me](mailto:mark@smallhadroncollider.com) first to make sure someone isn't already working on it.


### Style

Use `pure` instead of `return`, `*>` instead of `>>`, and `<>` instead of `++`.

Please use [`hfmt`](https://github.com/danstiner/hfmt) to format files.

### Git

Please use the [git-flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model. You should only commit to `feature/*` branches: **do not commit to `develop` or `master` directly**. This will keep commit history simpler and make handling pull requests easier.

Please also use the `--rebase` and `--ff-only` options when running `git pull` - although if you're on your own `feature/*` branch this shouldn't make any difference.

### Testing

Please add tests for any new functionality and make sure that all current tests pass.
