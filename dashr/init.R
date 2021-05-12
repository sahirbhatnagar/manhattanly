# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption("repos")
r["CRAN"] <- "http://cloud.r-project.org"
options(repos=r)

if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(dash)
pacman::p_load(dashCoreComponents)
pacman::p_load(dashHtmlComponents)
pacman::p_load_gh("plotly/dashDaq")
pacman::p_load(dashBio)
pacman::p_load(manhattanly)
pacman::p_load(anytime)
pacman::p_load(dashTable)
pacman::p_load(jsonlite)

# remotes::install_github("plotly/dash-table")
# remotes::install_github("plotly/dash-html-components")
# remotes::install_github("plotly/dash-core-components")
# remotes::install_github("plotly/dashR")
# 
# remotes::install_github("plotly/dash-bio", dependencies=TRUE)
# remotes::install_github("plotly/dash-daq", dependencies=TRUE)

