#' Creates a plotly manhattan plot
#'
#' Creates a manhattan plot from PLINK assoc output (or any data frame with
#' chromosome, position, and p-value).
#'
#' @param col A character vector indicating which colors to alternate.
#' @param chrlabs A character vector equal to the number of chromosomes
#'   specifying the chromosome labels (e.g., \code{c(1:22, "X", "Y", "MT")}).
#' @param suggestiveline Where to draw a "suggestive" line. Default
#'   -log10(1e-5). Set to FALSE to disable.
#' @param suggestiveline_color color of "suggestive" line. Only used if
#'   \code{suggestiveline} is not set to \code{FALSE}
#' @param genomewideline Where to draw a "genome-wide sigificant" line. Default
#'   -log10(5e-8). Set to FALSE to disable.
#' @param genomewideline_color color of "genome-wide sigificant" line. Only used if
#'   \code{genomewideline} is not set to \code{FALSE}
#' @param highlight A character vector of SNPs in your dataset to highlight.
#'   These SNPs should all be in your dataset.
#' @param showlegend Should a legend be shown. Default is \code{TRUE}.
#' @param ... Currently not used.
#'
#' @return A manhattan plot.
#'
#' @aliases
#' manhattanly.default
#' manhattanly.manhattanr
#' @export
#' @examples
#' \dontrun{
#' library(qqman)
#' manhattanly(gwasResults)
#' }

manhattanly <- function(x,
                        col = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="Set1")))(nchr),
                        chrlabs = NULL,
                        suggestiveline = -log10(1e-5),
                        genomewideline = -log10(5e-8),
                        suggestiveline_color = "blue",
                        genomewideline_color = "red",
                        highlight = NULL,
                        showlegend = TRUE,
                        ...) {

  UseMethod("manhattanly")

}

#' @export
manhattanly.default <- function(x,
                                col = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="Set1")))(nchr),
                                chrlabs = NULL,
                                suggestiveline = -log10(1e-5),
                                genomewideline = -log10(5e-8),
                                suggestiveline_color = "blue",
                                genomewideline_color = "red",
                                highlight = NULL,
                                showlegend = TRUE,
                                ...) {

  mh <- manhattanr(x, ...)
  manhattanly.manhattanr(mh,
                         col = col,
                         chrlabs = chrlabs,
                         suggestiveline = suggestiveline,
                         genomewideline = genomewideline,
                         suggestiveline_color = suggestiveline_color,
                         genomewideline_color = genomewideline_color,
                         highlight = highlight,
                         showlegend = showlegend)
}


#' @export
manhattanly.manhattanr <- function(x,
                                   col = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="Set1")))(nchr),
                                   chrlabs = NULL,
                                   suggestiveline = -log10(1e-5),
                                   genomewideline = -log10(5e-8),
                                   suggestiveline_color = "blue",
                                   genomewideline_color = "red",
                                   highlight = NULL,
                                   highlight_color = "green",
                                   showlegend = TRUE,
                                   showgrid = TRUE,
                                   xlab,
                                   ylab = "-log10(p)",
                                   title = "Manhattan Plot",
                                   ...) {

  x <- manhattanr(gwasResults)
  chrlabs <- NULL
  col <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="Set1")))(22)
  showgrid <- TRUE
  chrlabs = NULL
  suggestiveline = -log10(1e-5)
  genomewideline = -log10(5e-8)
  suggestiveline_color = "blue"
  genomewideline_color = "red"
  highlight_color = "green"
  highlight = NULL
  showlegend = TRUE
  showgrid = TRUE
  ylab = "-log10(p)"
  title = "Manhattan Plot"

  #########
  d <- x$data
  labs <- x$labs
  xlabel <- x$xlabel
  ticks <- x$ticks
  nchr <- x$nchr

  # Initialize plot
  xmax = ceiling(max(d$pos) * 1.03)
  xmin = floor(max(d$pos) * -0.03)

  # If manually specifying chromosome labels, ensure a character vector
  # and number of labels matches number chrs.
  if (!is.null(chrlabs)) {
    if (is.character(chrlabs)) {
      if (length(chrlabs)==length(labs)) {
        labs <- chrlabs
      } else {
        warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
      }
    } else {
      warning("If you're trying to specify chromosome labels, chrlabs must be a character vector")
    }
  }

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Initalize plotly
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(p)
  p <- plot_ly()

  # Add an axis.
  if (nchr == 1) {
    #If single chromosome, ticks and labels automatic.
    p %<>% layout(p,
                  title = title,
                  xaxis = list(
                    #title = if(!is.missing(xlab)) xlab else xlabel,
                    title = "ll",
                    showgrid = showgrid,
                    range = c(xmin, xmax)
                  ),
                  yaxis = list(
                    title = ylab,
                    range = c(0,ceiling(max(d$logp)))
                  )
    )
  } else {
    # if multiple chrs, use the ticks and labels you created above.
    p %<>% layout(p,
                title = title,
                xaxis = list(
                  # title = if(!missing(xlab)) xlab else "Chromosome",
                  title = "ll",
                  showgrid = showgrid,
                  range = c(xmin, xmax),
                  autotick = FALSE,
                  tickmode = "array",
                  tickvals = ticks,
                  ticktext = labs,
                  ticks = "outside"
                ),
                yaxis = list(
                  title = ylab,
                  range = c(0,ceiling(max(d$logp)))
                )
    )
  }

  # Create a vector of alternatiting colors
  col <- rep(col, max(d$CHR))

  # Add points to the plot
  if (nchr==1) {

    p %<>% add_trace(x = d$pos, y = d$logp,
                     type = "scatter",
                     mode = "markers",
                     evaluate = TRUE,
                     text = d$SNP,
                     marker = list(color = col[1]),
                     name = paste0("chr", unique(d$CHR)))

  } else {

    icol <- 1

    for(i in unique(d$index)) {

      tmp <- d[d$index == unique(d$index)[i], ]

      # get chromosome name for labeling
      chromo <- unique(tmp[which(tmp$index==i),"CHR"])
      p %<>% add_trace(x = tmp$pos, y = tmp$logp, type = "scatter",
                       mode = "markers", evaluate = TRUE,
                       text = tmp$SNP,
                       marker = list(color = col[icol]),
                       name = paste0("chr",chromo))
      icol = icol + 1
    }

  }

  if (suggestiveline & genomewideline) {p %<>% layout(p,
                                     shapes = list(
                                       list(type = "line",
                                            fillcolor = suggestiveline_color,
                                            line = list(color = suggestiveline_color),
                                            x0 = xmin, x1 = xmax, xref = "x",
                                            y0 = suggestiveline, y1 = suggestiveline, yref = "y"),
                                       list(type = "line",
                                            fillcolor = genomewideline_color,
                                            line = list(color = genomewideline_color),
                                            x0 = xmin, x1 = xmax, xref = "x",
                                            y0 = genomewideline, y1 = genomewideline, yref = "y")
                                     ))}

  if (suggestiveline & !(genomewideline)) {p %<>% layout(p,
                                                      shapes = list(
                                                        list(type = "line",
                                                             fillcolor = suggestiveline_color,
                                                             line = list(color = suggestiveline_color),
                                                             x0 = xmin, x1 = xmax, xref = "x",
                                                             y0 = suggestiveline, y1 = suggestiveline, yref = "y")
                                                      ))}

  if (!(suggestiveline) & genomewideline) {p %<>% layout(p,
                                                      shapes = list(
                                                        list(type = "line",
                                                             fillcolor = genomewideline_color,
                                                             line = list(color = genomewideline_color),
                                                             x0 = xmin, x1 = xmax, xref = "x",
                                                             y0 = genomewideline, y1 = genomewideline, yref = "y")
                                                      ))}

  # Highlight snps from a character vector
  if (!is.null(highlight)) {
    if (any(!(highlight %in% d$SNP))) warning("You're trying to highlight SNPs that don't exist in your results.")

    d.highlight <- d[which(d$SNP %in% highlight), ]

    p %<>% add_trace(p, x = d.highlight$pos,
                     y = d.highlight$logp,
                     type = "scatter",
                     mode = "markers",
                     evaluate = TRUE,
                     text = d.highlight$SNP,
                     marker = list(color = highlight_color),
                     name = "of interest")
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

