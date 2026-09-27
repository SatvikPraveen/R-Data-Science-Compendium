# Learning modules

These scripts are standalone, tutorial-style walkthroughs of R programming and
data science topics. They are **not** part of the installable package in
[`R/`](../R); they are meant to be read and run interactively, e.g.

```r
source("modules/04-statistical-analysis/hypothesis-testing.R")
```

Each script attaches the packages it needs with `library()`; nothing is
installed as a side effect of sourcing. To install everything the modules,
case studies and Shiny apps use, and then check that they all run:

```sh
Rscript modules/install-dependencies.R   # discovers ~150 packages from the code
Rscript modules/check-modules.R          # scripts | reports | apps (default: all)
```

`check-modules.R` sources each script in a fresh R session and calls its
`run_*_demo()` functions, renders each case study, and starts each Shiny app's
server with `shiny::testServer()`. It runs in CI on every change and weekly.

The case studies use **simulated data** with a fixed seed and a fixed
analysis date, and every number they report is computed in the document.

The tested, documented implementations of the core statistical methods live
in the package itself (see the top-level README).

| Directory                 | Topics                                                   |
| ------------------------- | -------------------------------------------------------- |
| `01-basics/`              | Data types, control flow, functions                      |
| `02-data-manipulation/`   | Base R, dplyr, data cleaning                             |
| `03-visualization/`       | Base graphics, ggplot2, themes, interactive plots        |
| `04-statistical-analysis/`| Descriptive statistics, hypothesis tests, regression, TS |
| `05-machine-learning/`    | Feature engineering, supervised/unsupervised learning    |
| `06-advanced-topics/`     | Functional programming, OOP, package dev, parallelism    |
| `utils/`                  | Helpers shared by the scripts above                      |
| `case-studies/`           | R Markdown case studies built on the modules             |
