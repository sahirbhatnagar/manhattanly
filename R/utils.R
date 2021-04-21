# This is where all utility functions should appear
# These functions are not exported

"%ni%" <- Negate("%in%")

my_vline <- function(x = 0, color = "grey", dash = "dash", width = 2) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, width = width, dash = dash)
  )
}

my_hline <- function(y = 0, color = "grey", dash = "dash", width = 2) {
  
  # https://plotly.com/r/reference/#layout-shapes for dash arguments
  # can be number (to keep original API or character)
  # dash <- match.arg(dash)
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, width = width, dash = dash)
  )
}


# a function to calculate your abline
p_abline <- function(x, a=1, b=0){
  y <- a * x + b
  return(y)
}