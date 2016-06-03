.onAttach <- function(...){
  # packageStartupMessage(manhattanlyWelcomeMessage())
  packageStartupMessage("See example usage at http://sahirbhatnagar.com/manhattanly/")
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
