# manhattanly 0.3.0 (2021-04-21)

## Major new features

* Both `qqly` and `volcanoly` are re-written to use the native `plotly` API, instead of relying on `plotly::ggplotly`
* Added tests for all main functions
* Added pkgdown website available at https://sahirbhatnagar.com/manhattanly/
* Removed rendered figures from vignette to reduce size of compiled package. Complete vignette can now be found on the website listed above.

## Bug fixes and minor improvements

* Fixed major bugs in `volcanor` and `volcanoly` (#11, #21, #22)


## Bug fixes and minor improvements

* By default, hover is based on the closest point rather than the x-coordinate (@mshadbolt, #1)
* `evaluate` argument deprecated (@thebioengineer, #4)
* Hover annotations for highlighted SNPs bug fixed (@mshadbolt, #6)
* Reduced size of `Introduction to manhattanly` vignette by using `html_vignette` (instead of `html_document`) which by default uses smaller plot sizes. See sahirbhatnagar.com/manhattanly for full vignette. 
* Fixed issue with hovering text when no annotation columns are provided: Problem variables: 'text' (@davemcg, #7)


# manhattanly 0.2.0 (2016-11-17)

## Major new features

* `volcanoly` function creates interactive volcano plots with automatic highlighting of points

## Bug fixes and minor improvements

* By default, hover is based on the closest point rather than the x-coordinate (@mshadbolt, #1)
* `evaluate` argument deprecated (@thebioengineer, #4)
* Hover annotations for highlighted SNPs bug fixed (@mshadbolt, #6)
* Reduced size of `Introduction to manhattanly` vignette by using `html_vignette` (instead of `html_document`) which by default uses smaller plot sizes. See sahirbhatnagar.com/manhattanly for full vignette. 
* Fixed issue with hovering text when no annotation columns are provided: Problem variables: 'text' (@davemcg, #7)


# manhattanly 0.1.0 (2016-06-03)

* Added a `NEWS.md` file to track changes to the package.
* Added vignette
* First version of the package



