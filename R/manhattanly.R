#' Creates a plotly manhattan plot
#'
#' Creates an interactive manhattan plot with multiple annotation options
#'
#' @param x Can be an object of class \code{manhattanr} produced by the
#'   \code{\link{manhattanr}} function or a \code{data.frame} which must contain
#'   at least the following three columns: \itemize{ \item{the chromosome
#'   number} \item{genomic base-pair position} \item{a numeric quantity to plot
#'   such as a p-value or zscore} }
#' @param col A character vector indicating the colors of each chromosome. If
#'   the number of colors specified is less than the number of unique
#'   chromosomes, then the elements will be recycled. Can be
#'   \href{http://www.rapidtables.com/web/color/RGB_Color.htm}{Hex Codes} as
#'   well.
#' @param point_size A \code{numeric} indicating the size of the points on the
#'   plot. Default is 5
#' @param labelChr A character vector equal to the number of chromosomes
#'   specifying the chromosome labels (e.g., \code{c(1:22, "X", "Y", "MT")}).
#'   Default is \code{NULL}, meaning that the actual chromosome numbers will be
#'   used.
#' @param suggestiveline Where to draw a "suggestive" line. Default is
#'   \code{-log10(1e-5)}. Set to \code{FALSE} to disable.
#' @param suggestiveline_color color of "suggestive" line. Only used if
#'   \code{suggestiveline} is not set to \code{FALSE}. Default is \code{"blue"}.
#' @param suggestiveline_width Width of \code{suggestiveline}. Default is 1.
#' @param genomewideline Where to draw a "genome-wide sigificant" line. Default
#'   \code{-log10(5e-8)}. Set to \code{FALSE} to disable.
#' @param genomewideline_color color of "genome-wide sigificant" line. Only used
#'   if \code{genomewideline} is not set to \code{FALSE}. Default is
#'   \code{"red"}.
#' @param genomewideline_width Width of \code{genomewideline}. Default is 1.
#' @param highlight A character vector of SNPs in your dataset to highlight.
#'   These SNPs should all be in your dataset. Default is \code{NULL} which
#'   means that nothing is highlighted.
#' @param highlight_color Color used to highlight points. Only used if
#'   \code{highlight} argument has been specified
#' @param showlegend Should a legend be shown. Default is \code{FALSE}.
#' @param showgrid Should gridlines be shown. Default is \code{FALSE}.
#' @param xlab X-axis label. Default is \code{NULL} which means that the label
#'   is automatically determined by the \code{\link{manhattanr}} function.
#'   Specify here to overwrite the default.
#' @param ylab Y-axis label. Default is \code{"-log10(p)"}.
#' @param title Title of the plot. Default is \code{"Manhattan Plot"}
#' @param ... other parameters passed to \code{\link{manhattanr}}
#' @inheritParams manhattanr
#' @note This package is inspired by the
#'   \href{https://github.com/stephenturner/qqman}{\code{qqman}} package by
#'   \href{http://www.gettinggeneticsdone.com/}{Stephen Turner}. Much of the
#'   plot format and pre-processing is the same. This package provides
#'   additional annotation options and builds on the \code{\link{plotly}}
#'   \code{d3.js} engine. These plots can be included in Shiny apps, Rmarkdown
#'   documents or embeded in websites using simple HTML code.
#' @return An interactive manhattan plot.
#' @seealso \code{\link{manhattanr}}, \code{\link{HapMap}},
#'   \code{\link{significantSNP}}, \code{\link[qqman]{manhattan}},
#'   \url{https://github.com/stephenturner/qqman},
#'   \href{https://github.com/nstrayer/D3ManhattanPlots}{D3ManhattanPlots}
#' @aliases manhattanly.default manhattanly.manhattanr
#' @import magrittr
#' @importFrom plotly plot_ly layout add_trace
#' @export
#' @examples
#' \dontrun{
#' library(manhattanly)
#' manhattanly(HapMap)
#'
#' # highlight SNPs of interest
#' # 'signigicantSNP' is a character vector of SNPs included in this package
#' manhattanly(HapMap, snp = "SNP", highlight = significantSNP)
#' }

manhattanly <- function(x,
                        # col = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(nchr),
                        # col = RColorBrewer::brewer.pal(n = 9, name = "Greys"),
                        col = c("#969696", "#252525"),
                        point_size = 5,
                        labelChr = NULL,
                        suggestiveline = -log10(1e-5),
                        suggestiveline_color = "blue",
                        suggestiveline_width = 1,
                        genomewideline = -log10(5e-8),
                        genomewideline_color = "red",
                        genomewideline_width = 1,
                        highlight = NULL,
                        highlight_color = "#00FF00",
                        showlegend = FALSE,
                        showgrid = FALSE,
                        xlab = NULL,
                        ylab = "-log10(p)",
                        title = "Manhattan Plot", ...) {

  UseMethod("manhattanly")

}

#' @export
manhattanly.default <- function(x,
                                # col = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(nchr),
                                # col = RColorBrewer::brewer.pal(n = 9, name = "Set1"),
                                col = c("#969696", "#252525"),
                                point_size = 5,
                                labelChr = NULL,
                                suggestiveline = -log10(1e-5),
                                suggestiveline_color = "blue",
                                suggestiveline_width = 1,
                                genomewideline = -log10(5e-8),
                                genomewideline_color = "red",
                                genomewideline_width = 1,
                                highlight = NULL,
                                highlight_color = "#00FF00",
                                showlegend = FALSE,
                                showgrid = FALSE,
                                xlab = NULL,
                                ylab = "-log10(p)",
                                title = "Manhattan Plot", ...) {

  mh <- manhattanr(x, ...)
  nchr <- mh$nchr
  manhattanly.manhattanr(mh,
                         col = col,
                         labelChr = labelChr,
                         point_size = point_size,
                         suggestiveline = suggestiveline,
                         suggestiveline_color = suggestiveline_color,
                         suggestiveline_width = suggestiveline_width,
                         genomewideline = genomewideline,
                         genomewideline_color = genomewideline_color,
                         genomewideline_width = genomewideline_width,
                         highlight = highlight,
                         highlight_color = highlight_color,
                         showlegend = showlegend,
                         showgrid = showgrid,
                         xlab = xlab,
                         ylab = ylab,
                         title = title)
}


#' @export
manhattanly.manhattanr <- function(x,
                                   # col = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(nchr),
                                   # col = RColorBrewer::brewer.pal(n = 9, name = "Set1"),
                                   col = c("#969696", "#252525"),
                                   point_size = 5,
                                   labelChr = NULL,
                                   suggestiveline = -log10(1e-5),
                                   suggestiveline_color = "blue",
                                   suggestiveline_width = 1,
                                   genomewideline = -log10(5e-8),
                                   genomewideline_color = "red",
                                   genomewideline_width = 1,
                                   highlight = NULL,
                                   highlight_color = "#00FF00",
                                   showlegend = FALSE,
                                   showgrid = FALSE,
                                   xlab = NULL,
                                   ylab = "-log10(p)",
                                   title = "Manhattan Plot",
                                   ...) {

  # x <- manhattanr(gwasResults)
  # x <- manhattanr(kk, annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")
  # x <- manhattanr(kk, annotation1 = "ZSCORE")
  # x <- manhattanr(kk, annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")
  # x <- manhattanr(HapMap, snp = "SNP")
  #
  # x$data %>% head
  # str(x$data)
  # labelChr <- NULL
  # col <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="Set1")))(22)
  # showgrid <- TRUE
  # labelChr = NULL
  # point_size = 5
  # suggestiveline = -log10(1e-5)
  # genomewideline = -log10(5e-8)
  # suggestiveline_color = "blue"
  # genomewideline_color = "red"
  # suggestiveline_width = genomewideline_width = 1;
  # highlight_color = "#00FF00"
  # highlight = significantSNP
  # showlegend = TRUE
  # showgrid = TRUE
  # ylab = "-log10(p)"
  # title = "Manhattan Plot"

  #########

  d <- x$data
  pName <- x$pName
  snpName <- x$snpName
  geneName <- x$geneName
  annotation1Name <- x$annotation1Name
  annotation2Name <- x$annotation2Name
  labs <- x$labs
  xlabel <- x$xlabel
  ticks <- x$ticks
  nchr <- x$nchr

  if (!is.null(highlight) & is.na(snpName)) stop("You're trying to highlight snps, but havent provided a snp column")

  # Initialize plot
  xmax = ceiling(max(d$pos) * 1.03)
  xmin = floor(max(d$pos) * -0.03)

  # If manually specifying chromosome labels, ensure a character vector
  # and number of labels matches number chrs.
  if (!is.null(labelChr)) {
    if (is.character(labelChr)) {
      if (length(labelChr)==length(labs)) {
        labs <- labelChr
      } else {
        warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
      }
    } else {
      warning("If you're trying to specify chromosome labels, labelChr must be a character vector")
    }
  }

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Initalize plotly
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  p <- plotly::plot_ly()

  # Add an axis.
  if (nchr == 1) {
    #If single chromosome, ticks and labels automatic.
    p %<>% plotly::layout(p,
                  title = title,
                  xaxis = list(
                    title = if(!is.null(xlab)) xlab else xlabel,
                    # title = "ll",
                    showgrid = showgrid,
                    range = c(xmin, xmax)
                  ),
                  yaxis = list(
                    title = ylab)#,
                    #range = c(0,ceiling(max(d$logp)))
                  #)
    )
  } else {
    # if multiple chrs, use the ticks and labels you created above.
    p %<>% plotly::layout(p,
                title = title,
                xaxis = list(
                  title = if(!is.null(xlab)) xlab else "Chromosome",
                  # title = "ll",
                  showgrid = showgrid,
                  range = c(xmin, xmax),
                  autotick = FALSE,
                  tickmode = "array",
                  tickvals = ticks,
                  ticktext = labs,
                  ticks = "outside"
                ),
                yaxis = list(
                  title = ylab)#,
                  #range = c(0,ceiling(max(d$logp)))
                #)
    )
  }

  # Create a vector of alternatiting colors
  col <- rep(col, max(d$CHR))

  # Add points to the plot
  if (nchr==1) {

    # paste(if (!is.na(snpName)) paste0(snpName,": ",d[[snpName]],"<br>"),
    # if (!is.na(geneName)) paste0(geneName,": ",d[[geneName]],"<br>"),
    # if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d[[annotation1Name]],"<br>")
    # if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d[[annotation2Name]],"<br>")

    TEXT <- paste(if (!is.na(snpName)) paste0(snpName,": ",d[[snpName]]),
                  if (!is.na(geneName)) paste0(geneName,": ",d[[geneName]]),
                  if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d[[annotation1Name]]),
                  if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d[[annotation2Name]]), sep = "<br>")

    p %<>% plotly::add_trace(x = d$pos, y = d$logp,
                     type = "scatter",
                     mode = "markers",
                     evaluate = TRUE,
                     text = TEXT,
                     showlegend = showlegend,
                     marker = list(color = col[1],
                                   size = point_size),
                     name = paste0("chr", unique(d$CHR)))

  } else {

    icol <- 1

    for(i in unique(d$index)) {

      tmp <- d[d$index == unique(d$index)[i], ]

      TEXT <- paste(if (!is.na(snpName)) paste0(snpName,": ", tmp[[snpName]]),
                    if (!is.na(geneName)) paste0(geneName,": ", tmp[[geneName]]),
                    if (!is.na(annotation1Name)) paste0(annotation1Name,": ", tmp[[annotation1Name]]),
                    if (!is.na(annotation2Name)) paste0(annotation2Name,": ", tmp[[annotation2Name]]),
                    sep = "<br>")

      # get chromosome name for labeling
      chromo <- unique(tmp[which(tmp$index==i),"CHR"])
      p %<>% plotly::add_trace(x = tmp$pos, y = tmp$logp, type = "scatter",
                       mode = "markers", evaluate = TRUE,
                       text = TEXT,
                       showlegend = showlegend,
                       marker = list(color = col[icol],
                                     size = point_size),
                       name = paste0("chr",chromo))
      icol = icol + 1
    }

  }

  if (suggestiveline & genomewideline) {p %<>% plotly::layout(p,
                                     shapes = list(
                                       list(type = "line",
                                            fillcolor = suggestiveline_color,
                                            line = list(color = suggestiveline_color,
                                                        width = suggestiveline_width),
                                            x0 = xmin, x1 = xmax, xref = "x",
                                            y0 = suggestiveline, y1 = suggestiveline, yref = "y"),
                                       list(type = "line",
                                            fillcolor = genomewideline_color,
                                            line = list(color = genomewideline_color,
                                                        width = genomewideline_width),
                                            x0 = xmin, x1 = xmax, xref = "x",
                                            y0 = genomewideline, y1 = genomewideline, yref = "y")
                                     ))}

  if (suggestiveline & !(genomewideline)) {p %<>% plotly::layout(p,
                                                      shapes = list(
                                                        list(type = "line",
                                                             fillcolor = suggestiveline_color,
                                                             line = list(color = suggestiveline_color,
                                                                         width = suggestiveline_width),
                                                             x0 = xmin, x1 = xmax, xref = "x",
                                                             y0 = suggestiveline, y1 = suggestiveline, yref = "y")
                                                      ))}

  if (!(suggestiveline) & genomewideline) {p %<>% plotly::layout(p,
                                                      shapes = list(
                                                        list(type = "line",
                                                             fillcolor = genomewideline_color,
                                                             line = list(color = genomewideline_color,
                                                                         width = genomewideline_width),
                                                             x0 = xmin, x1 = xmax, xref = "x",
                                                             y0 = genomewideline, y1 = genomewideline, yref = "y")
                                                      ))}

  # Highlight snps from a character vector
  if (!is.na(snpName)) {
    if (!is.null(highlight)) {
      if (any(!(highlight %in% d[[snpName]]))) warning("You're trying to highlight SNPs that don't exist in your results.")

      d.highlight <- d[which(d[[snpName]] %in% highlight), ]

      p %<>% plotly::add_trace(x = d.highlight$pos,
                       y = d.highlight$logp,
                       type = "scatter",
                       mode = "markers",
                       evaluate = TRUE,
                       text = d.highlight[[snpName]],
                       showlegend = showlegend,
                       marker = list(color = highlight_color,
                                     size = point_size),
                       name = "of interest")
    }
  }
  p
}

# jj <- manhattan_plotly(gwasResults, genomewideline = FALSE)
#
# jj
# str(jj)

# topHits = subset(d, P <= annotatePval)
# p %>% layout(annotations = list(x = topHits$pos[10],
#                                 y = -log10(topHits$P[10]),
#                                 text = topHits$SNP[10],
#                                 showarrow = T))

"%ni%" <- Negate("%in%")
