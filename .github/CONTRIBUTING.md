# Contributing to rmo

This outlines how to propose a change to the `rmo` package.

## Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, as long as the changes are made in the *source* file.

When fixing typos, please make sure to describe the change in the commit
message. For example

```markdown
docs: fix typo in the introduction section
```

:warning: You edit a roxygen comment in a `.R` file below `R/` and not in an
`.Rd` file below `man/`.

## Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure maintainers agree that it’s a problem. If you’ve found a bug, create
an associated issue and illustrate the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

## Coding style

To ensure consistent coding style, we utilize the following tools for style
enforcement:

- [EditorConfig](https://editorconfig.org/) for maintaining consistent
  whitespace usage.
- [lintr](https://lintr.r-lib.org) for enforcing R coding style.
- [ClangFormat](https://clang.llvm.org/docs/ClangFormat.html) for maintaining
  consistent C++ coding style.

## Commit guidelines

We use the conventional commits specification for our commit messages. This means that your commit message should be structured as follows:

```markdown
<type>[optional scope]: <description>

[optional body]

[optional footer]
```

For more information, see the [conventional commits specification](https://www.conventionalcommits.org).

## Branching strategy

We use the GitHub Flow branching strategy. This means that each feature or bug
fix is developed in a separate branch. When working on a new feature or fixing a
bug, create a new branch from the main branch. Once the changes are complete,
submit a pull request to merge the branch back into the main branch. The pull
request will be reviewed by the maintainers before merging. This approach allows
for better collaboration and ensures that changes are thoroughly reviewed before
being merged into the main codebase.

## Testing and code coverage

We use the packages [`testthat`](https://testthat.r-lib.org) for unit testing and [`covr`](https://covr.r-lib.org) for checking code coverage.

Run all tests with the following command:

```r
devtools::test()
```

Check the code coverage with the following command:

```r
covr::package_coverage()
```

## Summary

- We recommend that you create a Git branch for each pull request (PR).
- New code should follow the tidyverse [style
  guide](https://style.tidyverse.org) and pass `lintr::lint_package()` without
  errors. You can use the [styler](https://styler.r-lib.org) package to apply
  these styles, but please don't restyle code that has nothing to do with your
  PR. For more information on the static code analysis tool `lintr`, see
  <https://lintr.r-lib.org>. For changes of the C++ backend, use ClangFormat.
- We use [roxygen2](https://roxygen2.r-lib.org), with [Markdown
  syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
  for documentation.  
- We use [testthat](https://testthat.r-lib.org). Contributions with test cases
  included are easier to accept.  
- For user-facing changes, add a bullet to the top of `NEWS.md` below the
  current development version header describing the changes made followed by
  your GitHub username, and links to relevant issue(s)/PR(s).

## Code of Conduct

Please note that the rmo project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
