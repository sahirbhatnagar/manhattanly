#!/usr/bin/env Rscript

devtools::install_github('rstudio/rmarkdown')
rmarkdown::render("index.Rmd", output_dir = "_book/")