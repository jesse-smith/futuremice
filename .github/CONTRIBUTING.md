# Contributing to futuremice

This outlines how to propose a change to `{futuremice}`. 
For more detailed info about contributing to this package, please see the
[**development contributing guide**](https://rstd.io/tidy-contrib). Note that the process is based on the tidyverse contributing guide, but [`{fledge}`](https://cynkra.github.io/fledge/) is used to keep track of the changelog (`NEWS.md`).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("jesse-smith/futuremice", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
    
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes and commit to git. For important user-facing changes, add a hyphen (`-`) or asterisk (`*`) to at the beginning of important commit messages These are the commit messages that'll be recorded in the changelog eventually! You may also wish to only include the message for your PR's merge/squash commit (see below). Exclude housekeeping parts of the message by typing them after a line `---`. For example:

```
- Add support for bla databases.
```

or

```
- Add support for bla databases.

---

Also tweak the CI workflow accordingly :sweat_smile:
```

*   Create a PR by running `usethis::pr_push()` and following the prompts in your browser. The title of your PR should briefly describe the change. If you have user-facing changes and haven't already marked them for documentation in the changelog, prefix the title with a hyphen (`-`) or asterisk (`*`) (see above). The body of your PR should contain `Fixes #issue-number`.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the futuremice project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
