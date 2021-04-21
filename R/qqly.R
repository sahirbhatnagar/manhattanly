#' Creates a plotly Q-Q plot
#'
#' Creates an interactive Q-Q plot with multiple annotation options
#'
#' @param x Can be an object of class \code{qqr} produced by the
#'   \code{\link{qqr}} function or a \code{data.frame} which must contain at
#'   least the following column: \itemize{ \item{a p-value, must be numeric} }
#' @param col A \code{character} indicating the color of the points. Can
#'   be \href{https://www.rapidtables.com/web/color/RGB_Color.html}{Hex Codes} as
#'   well.
#' @param size A \code{numeric} specifying the size of the points. Default
#'   is 1
#' @param type An integer between 0 and 25 specifying the point shape.
#'   Default is 20 (filled circle). Deprecated.
#' @param abline_col A \code{character} indicating the color of the 45 degree
#'   diagonal line. Can be
#'   \href{https://www.rapidtables.com/web/color/RGB_Color.html}{Hex Codes} as
#'   well. Default is \code{"red"}.
#' @param abline_size A \code{numeric} indicating the size of the 45 degree
#'   diagonal line. Default is 0.5.
#' @param abline_type Sets the line type of the
#'   45 degree line. Set to a dash type character among "solid", "dot",
#'   "dash", "longdash", "dashdot", or "longdashdot", or a dash length list in
#'   px (eg "5px","10px","2px"). Can also be a positive numeric value (e.g 5,
#'   10, 2). Default is "dash". See
#'   \href{https://plotly.com/r/reference/#layout-shapes}{plotly help page on
#'   layouts} for complete list and more details
#' @param highlight A character vector of SNPs in your dataset to highlight.
#'   These SNPs should all be in your dataset. Default is \code{NULL} which
#'   means that nothing is highlighted.
#' @param highlight_color Color used to highlight points. Only used if
#'   \code{highlight} argument has been specified
#' @param xlab X-axis label. Default is \code{"Expected -log10(p)"}
#' @param ylab Y-axis label. Default is \code{"Observed -log10(p)"}
#' @param title Title of the plot. Default is \code{"Q-Q Plot"}
#' @param ... other parameters passed to \code{\link{qqr}}
#' @return An interactive Q-Q plot.
#' @aliases qqly.default qqly.qqr
#' @seealso \code{\link{qqr}}, \code{\link{HapMap}},
#'   \code{\link{significantSNP}}
#' @export
#' @importFrom magrittr '%>%'
#' @examples
#' \dontrun{
#' library(manhattanly)
#' qqly(HapMap)
#'
#' # highlight SNPs of interest
#' # 'signigicantSNP' is a character vector of SNPs included in this package
#' qqly(HapMap, snp = "SNP", highlight = significantSNP)
#' }
#'
qqly <- function(x,
                 ...,
                 col = "#252525",
                 size = 5,
                 type = 20,
                 abline_col = "red",
                 abline_size = 1,
                 abline_type = "solid",
                 highlight = NULL,
                 highlight_color = "#00FF00",
                 xlab = "Expected -log10(p)",
                 ylab = "Observed -log10(p)",
                 title = "Q-Q Plot") {
  UseMethod("qqly")
}

#' @export
qqly.default <- function(x,
                         ...,
                         col = "#252525",
                         size = 5,
                         type = 20,
                         abline_col = "red",
                         abline_size = 1,
                         abline_type = "solid",
                         highlight = NULL,
                         highlight_color = "#00FF00",
                         xlab = "Expected -log10(p)",
                         ylab = "Observed -log10(p)",
                         title = "Q-Q Plot") {
  qq <- qqr(x, ...)
  qqly.qqr(qq,
    col = col,
    size = size,
    type = type,
    abline_col = abline_col,
    abline_size = abline_size,
    abline_type = abline_type,
    highlight = highlight,
    highlight_color = highlight_color,
    xlab = xlab,
    ylab = ylab,
    title = title
  )
}


#' @export
qqly.qqr <- function(x,
                     ...,
                     col = "#252525",
                     size = 5,
                     type = 20,
                     abline_col = "red",
                     abline_size = 1,
                     abline_type = "solid",
                     highlight = NULL,
                     highlight_color = "#00FF00",
                     xlab = "Expected -log10(p)",
                     ylab = "Observed -log10(p)",
                     title = "Q-Q Plot") {
  d <- x$data
  pName <- x$pName
  snpName <- x$snpName
  geneName <- x$geneName
  annotation1Name <- x$annotation1Name
  annotation2Name <- x$annotation2Name
  observed_vector <- d[["OBSERVED"]]
  expected_vector <- d[["EXPECTED"]]

  # used for identity line
  for (i in 1:length(expected_vector)) {
    if (i == 1 || max(expected_vector[[i]]) > max_x) {
      max_x <- max(expected_vector[[i]])
    }
    if (i == 1 || max(observed_vector[[i]]) > max_y) {
      max_y <- max(observed_vector[[i]])
    }
    if (i == 1 || min(expected_vector[[i]]) < min_x) {
      min_x <- min(expected_vector[[i]])
    }
    if (i == 1 || min(observed_vector[[i]]) < min_y) {
      min_y <- min(observed_vector[[i]])
    }
  }


  if (!is.null(highlight) & is.na(snpName)) stop("You're trying to highlight snps, but havent provided a snp column")

  my_xlab <- list(
    title = xlab
  )

  my_ylab <- list(
    title = ylab
  )

  fig <- plotly::plot_ly(d,
    x = ~EXPECTED, y = ~OBSERVED,
    type = "scatter", mode = "markers",
    hoverinfo = "text",
    marker = list(
      color = col[1],
      size = size
    ),
    text = ~ paste0(
      if (!is.na(snpName)) paste0(snpName, ": ", d[[snpName]], "<br>"),
      if (!is.na(geneName)) paste0(geneName, ": ", d[[geneName]], "<br>"),
      if (!is.na(annotation1Name)) paste0(annotation1Name, ": ", d[[annotation1Name]], "<br>"),
      if (!is.na(annotation2Name)) paste0(annotation2Name, ": ", d[[annotation2Name]], "<br>"),
      paste0("Observed", ": ", OBSERVED, "<br>"),
      paste0("Expected", ": ", EXPECTED)
    )
  ) %>%
    plotly::layout(xaxis = my_xlab, yaxis = my_ylab, title = list(text = title)) %>%
    plotly::layout(
      shapes = list(
        type = "line",
        x0 = min_x,
        x1 = max_x,
        y0 = p_abline(min_x),
        y1 = p_abline(max_x),
        line = list(color = abline_col, dash = abline_type, width = abline_size)
      )
    )

  # EXPECTED=OBSERVED=NULL
  # Highlight snps from a character vector
  if (!is.na(snpName)) {
    if (!is.null(highlight)) {
      if (any(!(highlight %in% d[[snpName]]))) warning("You're trying to highlight SNPs that don't exist in your results.")

      d.highlight <- d[which(d[[snpName]] %in% highlight), ]


      fig %<>% plotly::add_trace(
        x = ~EXPECTED,
        y = ~OBSERVED,
        data = d.highlight,
        inherit = FALSE,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = highlight_color,
          size = size
        ),
        showlegend = FALSE,
        text = ~ paste0(
          if (!is.na(snpName)) paste0(snpName, ": ", d.highlight[[snpName]], "<br>"),
          if (!is.na(geneName)) paste0(geneName, ": ", d.highlight[[geneName]], "<br>"),
          if (!is.na(annotation1Name)) paste0(annotation1Name, ": ", d.highlight[[annotation1Name]], "<br>"),
          if (!is.na(annotation2Name)) paste0(annotation2Name, ": ", d.highlight[[annotation2Name]], "<br>"),
          paste0("Observed", ": ", OBSERVED, "<br>"),
          paste0("Expected", ": ", EXPECTED)
        )
      )
    }
  }


  fig
}
