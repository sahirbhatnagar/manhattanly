.onAttach <- function(...){
  # packageStartupMessage(manhattanlyWelcomeMessage())
  packageStartupMessage()
  packageStartupMessage("For example usage please run: vignette('manhattanly')")
  packageStartupMessage()
  packageStartupMessage("This package is largely based on the 'qqman' package by Stephen Turner. Please cite:")
  packageStartupMessage("Turner, S.D. qqman: an R package for visualizing GWAS results using Q-Q and manhattan plots. biorXiv DOI: 10.1101/005165 (2014).")
  packageStartupMessage()
}



# from https://github.com/talgalili/heatmaply/blob/master/R/zzz.R
# manhattanlyWelcomeMessage <- function(){
#
#     paste0("\n",
#          "---------------------\n",
#          "Welcome to manhattanly version ", utils::packageDescription("manhattanly")$Version, "\n",
#          # "\n",
#          "Type ?manhattanly for the main documentation.\n",
#          "The github page is: https://github.com/sahirbhatnagar/manhattanly/\n",
#          "\n",
#          "Suggestions and bug-reports can be submitted at: https://github.com/sahirbhatnagar/manhattanly/issues\n",
#          "Or contact: <sahir.bhatnagar@gmail.com>\n",
#          "\n",
#          "\tTo suppress this message use:  ", "suppressPackageStartupMessages(library(manhattanly))\n",
#          "---------------------\n"
#   )
# }
