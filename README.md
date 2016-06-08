[![Travis-CI Build Status](https://travis-ci.org/sahirbhatnagar/manhattanly.svg?branch=master)](https://travis-ci.org/sahirbhatnagar/manhattanly)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/manhattanly)](https://cran.r-project.org/package=manhattanly)

<!--
![](http://cranlogs.r-pkg.org/badges/manhattanly?color=yellow)
![](http://cranlogs.r-pkg.org/badges/grand-total/manhattanly?color=yellowgreen)
-->

# manhattanly

The goal of manhattanly is to create interactive manhattan and Q-Q plots

![](http://i.imgur.com/n88LCky.gif)

## Installation

You can install `manhattanly` from CRAN:

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

This package is inspired by the [`qqman`](https://github.com/stephenturner/qqman) by [Stephen Turner](http://stephenturner.us/). The pre-processing of the data in the `manhattanly` package is based on the `qqman::manhattan` and `qqman::qq` functions. 

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
