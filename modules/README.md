# Learning modules

These scripts are standalone, tutorial-style walkthroughs of R programming and
data science topics. They are **not** part of the installable package in
[`R/`](../R); they are meant to be read and run interactively, e.g.

```r
source("modules/04-statistical-analysis/hypothesis-testing.R")
```

Each script loads its own dependencies, so you may need to install extra
packages first. The tested, documented, research-grade implementations of the
core methods live in the package itself (see the top-level README).

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
