# Contributing

Bond welcomes contributions from the community.

## Process

1. Make a proposal.
1. Implement the proposal and its tests.
1. Rebase commits to tell a compelling story.
1. Start a pull request & address comments.
1. Merge.

### Proposal

For things like fixing typos and small bug fixes, you can skip this step.

If your change is more than a simple fix, please don't just create a big
pull request. Instead, start by opening an issue describing the problem you
want to solve and how you plan to approach the problem. This will let us
have a brief discussion about the problem and, hopefully, identify some
potential pitfalls before too much time is spent.

### Implementation

* Fork the repository on GitHub.
* Start on a new topic branch off of master.
* Instructions for getting Bond building and running the tests are in the
  [README](https://github.com/Microsoft/bond/blob/master/README.md).
* Make small and atomic commits that include tests.
* Make sure that all the tests continue to pass.
  * The CMake `check` target will run the C++ tests for you.
  * The C# unit tests can be run from
    [the command line](https://github.com/Microsoft/bond/blob/56b5914a5bb41178521e01f6ce078d429e3e6b71/appveyor.yml#L214)
    or from within Visual Studio.
* Update the [changelog](https://github.com/Microsoft/bond/blob/master/CHANGELOG.md).

### Rebase commits

The commits in your pull request should tell a story about how the code got
from point A to point B. Good stories are edited, so you'll want to rebase
your commits so that they tell a good story. No "add missing semi-colon"
commits, please. :-)

Each commit should build and pass all of the tests. If you want to add new
tests for functionality that's not yet written, ensure the tests are added
disabled.

* For Haskell tests, add the tests functions, but don't add a test case for
  them.
* For CTest tests, add the executable to the CMakeLists.txt file to ensure
  that it builds, but do not include a `add_test()` (or similar) entry for
  it.
* For C# NUnit tests, use the
  [`[Ignore]`](http://www.nunit.org/index.php?p=ignore&r=2.6.4) attribute to
  annotate the test cases to be ignored.

Don't forget to run `git diff --check` to catch those annoying whitespace
changes.

Please follow the established Git
[convention for commit messages](https://www.git-scm.com/book/en/v2/Distributed-Git-Contributing-to-a-Project#Commit-Guidelines).
The first line is a summary in the imperative, about 50 characters or less,
and should *not* end with a period. An optional, longer description must be
preceded by an empty line and should be wrapped at around 72 characters.
This helps with various outputs from Git or other tools.

You can update message of local commits you haven't pushed yet using `git
commit --amend` or `git rebase --interactive` with *reword* command.

### Pull request

Start a GitHub pull request to merge your topic branch into the
[main repository's master branch](https://github.com/Microsoft/bond/tree/master).
(If you are a Microsoft employee and are not a member of the
[Microsoft organization on GitHub](https://github.com/Microsoft) yet, please
contact the Bond Development team via e-mail for instructions before
starting your pull request. There's some process stuff you'll need to do
ahead of time.)

If you haven't contributed to a Microsoft project before, you may be asked
to sign a [contribution license agreement](https://cla.microsoft.com/). A
comment in the PR will let you know if you do.

The project maintainers will review your changes. We aim to review all
changes within three business days.

Address any review comments, force push to your topic branch, and post a
comment letting us know that there's new stuff to review.

### Merge

If the pull request review goes well, a project maintainer will merge your
changes. Thank you for helping improve Bond!
