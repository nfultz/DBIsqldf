# DBIsqldf

Provides an easy way to perform SQL selects on R data frames.

The package contains a single function `sqldf()` whose help file
contains more information and examples.

This package is a fork from the [original sqldf-package](https://github.com/ggrothendieck/sqldf).

The primary differences are:

* DBI interface only. This increases compatibility with database drivers, but at the expense
  of database-specific convenience features.
* DBI native parameterized query support.
* More conservative defaults for writing tables - data frames are imported as temporary tables
  and should be committed explicitly is they need to be persisted.
* No legacy (transitive) dependencies.

## Installation

```{r}
remotes::install_github("nfultz/DBIsqldf")
```
