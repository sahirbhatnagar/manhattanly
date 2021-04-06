<!-- badges: start -->
[![R-CMD-check](https://github.com/sahirbhatnagar/manhattanly/workflows/R-CMD-check/badge.svg)](https://github.com/sahirbhatnagar/manhattanly/actions)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/manhattanly?color=green)](https://www.r-pkg.org/pkg/manhattanly)
[![CRAN](https://www.r-pkg.org/badges/version/manhattanly?color=blue)](https://cran.r-project.org/package=manhattanly)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test coverage](https://codecov.io/gh/sahirbhatnagar/manhattanly/branch/master/graph/badge.svg)](https://codecov.io/gh/sahirbhatnagar/manhattanly?branch=master)
<!-- badges: end -->


<!--
![](http://cranlogs.r-pkg.org/badges/manhattanly?color=yellow)
![](http://cranlogs.r-pkg.org/badges/grand-total/manhattanly?color=yellowgreen)
-->

# manhattanly

The goal of manhattanly is to create interactive manhattan, Q-Q and volcano plots

![](http://i.imgur.com/n88LCky.gif)

## Installation

You can install `manhattanly` from [CRAN](https://cran.r-project.org/package=manhattanly):

```R
install.packages("manhattanly")
```

Alternatively, you can install the development version of `manhattanly` from [GitHub](https://github.com/sahirbhatnagar/manhattanly) with:

```R
install.packages("devtools")
devtools::install_github("sahirbhatnagar/manhattanly", build_vignettes = TRUE)
```

## Vignette

See the [online vignette](http://sahirbhatnagar.com/manhattanly/) for example usage of the functions.

## Credit

This package is inspired by the [`qqman`](https://github.com/stephenturner/qqman) package by [Stephen Turner](http://stephenturner.us/). The pre-processing of the data in the `manhattanly` package is based on the `qqman::manhattan` and `qqman::qq` functions. 

The splitting of the tasks into data pre-processing and plot rendering is inspired by the [`heatmaply`](https://github.com/talgalili/heatmaply) package by [Tal Galili](http://www.r-statistics.com/)


## Related Work

* [`qqman`](https://github.com/stephenturner/qqman)
* [`D3ManhattanPlots`](https://github.com/nstrayer/D3ManhattanPlots)






## Contact

* Issues: <https://github.com/sahirbhatnagar/manhattanly/issues>
* Pull Requests: <https://github.com/sahirbhatnagar/manhattanly/>
* e-mail: <sahir.bhatnagar@gmail.com>


## Latest news

You can see the most recent changes to the package in the [NEWS.md file](https://github.com/sahirbhatnagar/manhattanly/blob/master/NEWS.md)




## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
