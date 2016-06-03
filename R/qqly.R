#' Creates a plotly Q-Q plot
#'
#' Creates an interactive Q-Q plot with multiple annotation options
#'
#' @param x Can be an object of class \code{qqr} produced by the
#'   \code{\link{qqr}} function or a \code{data.frame} which must contain at
#'   least the following column: \itemize{ \item{a p-value, must be numeric} }
#' @param col A \code{character} indicating the color of the points. Can
#'   be \href{http://www.rapidtables.com/web/color/RGB_Color.htm}{Hex Codes} as
#'   well.
#' @param size A \code{numeric} specifying the size of the points. Default
#'   is 1
#' @param type An integer between 0 and 25 specifying the point shape.
#'   Default is 20 (filled circle). See
#'   \href{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}{R Cookbook}
#'   for complete list
#' @param abline_col A \code{character} indicating the color of the 45 degree
#'   diagonal line. Can be
#'   \href{http://www.rapidtables.com/web/color/RGB_Color.htm}{Hex Codes} as
#'   well. Default is \code{"red"}.
#' @param abline_size A \code{numeric} indicating the size of the 45 degree
#'   diagonal line. Default is 0.5.
#' @param abline_type An integer between 0 and 6 specifying the line type of the
#'   diagonal 45 degree line. Default is 1 (solid line). See
#'   \href{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}{R Cookbook}
#'   for complete list
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
#'   \code{\link{significantSNP}}, \code{\link[qqman]{qq}},
#'   \url{https://github.com/stephenturner/qqman}
#' @note This function first creates a \code{ggplot2} object and then converts
#'   it to a \code{plotly} object using \code{\link[plotly]{ggplotly}}
#' @import ggplot2
#' @export
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
                 col = "#252525",
                 size = 1,
                 type = 20,
                 abline_col = "red",
                 abline_size = 0.5,
                 abline_type = 1,
                 highlight = NULL,
                 highlight_color = "#00FF00",
                 xlab = "Expected -log10(p)",
                 ylab = "Observed -log10(p)",
                 title = "Q-Q Plot",
                 ...) {

  UseMethod("qqly")

}

#' @export
qqly.default <- function(x,
                         col = "#252525",
                         size = 1,
                         type = 20,
                         abline_col = "red",
                         abline_size = 0.5,
                         abline_type = 1,
                         highlight = NULL,
                         highlight_color = "#00FF00",
                         xlab = "Expected -log10(p)",
                         ylab = "Observed -log10(p)",
                         title = "Q-Q Plot",
                         ...) {

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
           title = title)
}


#' @export
qqly.qqr <- function(x,
                     col = "#252525",
                     size = 1,
                     type = 20,
                     abline_col = "red",
                     abline_size = 0.5,
                     abline_type = 1,
                     highlight = NULL,
                     highlight_color = "#00FF00",
                     xlab = "Expected -log10(p)",
                     ylab = "Observed -log10(p)",
                     title = "Q-Q Plot",
                     ...) {

  # x <- qqr(HapMap, snp = "SNP")
  # x$data %>% head
  # str(x$data)
  # # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  # col = "#252525"
  # size = 1
  # type = 20
  # abline_col = "red"
  # abline_size = 0.5
  # abline_type = 3
  # ticklabel_size = 10
  # xlab = "Expected -log10(p)"
  # xlab_size = 14
  # ylab = "Observed -log10(p)"
  # ylab_size = 14
  # title = "Q-Q Plot"
  # highlight = significantSNP
  # highlight_color = "#00FF00"

  #########

  d <- x$data
  pName <- x$pName
  snpName <- x$snpName
  geneName <- x$geneName
  annotation1Name <- x$annotation1Name
  annotation2Name <- x$annotation2Name

  if (!is.null(highlight) & is.na(snpName)) stop("You're trying to highlight snps, but havent provided a snp column")


  TEXT <- paste(if (!is.na(snpName)) paste0(snpName,": ", d[[snpName]]),
                if (!is.na(geneName)) paste0(geneName,": ", d[[geneName]]),
                if (!is.na(annotation1Name)) paste0(annotation1Name,": ", d[[annotation1Name]]),
                if (!is.na(annotation2Name)) paste0(annotation2Name,": ", d[[annotation2Name]]),
                sep = "<br>")

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Initalize ggplot and then convert to plotly using ggplotly
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  if (any(c(!is.na(snpName),!is.na(geneName),!is.na(annotation1Name), !is.na(annotation2Name)))) {
  p <- ggplot2::ggplot(data = d, aes_string(x = 'EXPECTED', y = 'OBSERVED')) +
    ggplot2::geom_point(aes(text = TEXT),
               size = size,
               color = col[1],
               shape = type) +
    ggplot2::geom_abline(aes(intercept = 0, slope = 1),
                size = abline_size,
                color = abline_col,
                linetype = abline_type) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggplot2::labs(x = xlab,
         y = ylab,
         title = title)
  } else {
    p <- ggplot2::ggplot(data = d, aes_string(x = 'EXPECTED', y = 'OBSERVED')) +
      ggplot2::geom_point(size = size,
                 color = col[1],
                 shape = type) +
      ggplot2::geom_abline(aes(intercept = 0, slope = 1),
                  size = abline_size,
                  color = abline_col,
                  linetype = abline_type) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ggplot2::labs(x = xlab,
           y = ylab,
           title = title)
  }

  # Highlight snps from a character vector
  if (!is.na(snpName)) {
    if (!is.null(highlight)) {
      if (any(!(highlight %in% d[[snpName]]))) warning("You're trying to highlight SNPs that don't exist in your results.")

      d.highlight <- d[which(d[[snpName]] %in% highlight), ]

      TEXT2 <- paste(if (!is.na(snpName)) paste0(snpName,": ", d.highlight[[snpName]]),
                    if (!is.na(geneName)) paste0(geneName,": ", d.highlight[[geneName]]),
                    if (!is.na(annotation1Name)) paste0(annotation1Name,": ", d.highlight[[annotation1Name]]),
                    if (!is.na(annotation2Name)) paste0(annotation2Name,": ", d.highlight[[annotation2Name]]),
                    sep = "<br>")

      p <- p + ggplot2::geom_point(data = d.highlight,
                          aes_string(x = 'EXPECTED', y = 'OBSERVED',
                              text = TEXT2),
                        size = size,
                        color = highlight_color,
                        shape = type)
    }
  }


  plotly::ggplotly(p)

}
