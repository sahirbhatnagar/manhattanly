#' Creates a manhattanr object
#' 
#' An object of class manhattanr includes all the needed information for 
#' producing a manhattan plot. In the same spirit as the \link{heatmaply} 
#' package, the goal is to seperate the pre-processing of the manhattan plot 
#' elements from the graphical rendaring of the object, which could be done 
#' using any graphical device including \code{\link[plotly]{plot_ly}} and 
#' \code{\link[graphics]{plot}} in base \code{R}.
#' 
#' @export
#' @source The pre-processing is largely based on the 
#'   \code{\link[qqman]{manhattan}} function from the 
#'   \href{https://github.com/stephenturner/qqman}{\code{qqman}} package by 
#'   \href{http://www.gettinggeneticsdone.com/}{Stephen Turner}
#'   
#' @seealso \code{\link[qqman]{manhattan}},
#'   \href{https://github.com/nstrayer/D3ManhattanPlots}{D3ManhattanPlots}