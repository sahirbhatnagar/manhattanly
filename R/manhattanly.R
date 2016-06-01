#' Creates a plotly manhattan plot
#'
#' Creates a manhattan plot from PLINK assoc output (or any data frame with
#' chromosome, position, and p-value).
#'
#' @param col A character vector indicating which colors to alternate.
#' @param labelChr A character vector equal to the number of chromosomes
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
#' @param ... other parameters passed to \link{manhattanr}
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
                        # col = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(nchr),
                        col = RColorBrewer::brewer.pal(n = 9, name = "Set1"),
                        labelChr = NULL,
                        suggestiveline = -log10(1e-5),
                        genomewideline = -log10(5e-8),
                        suggestiveline_color = "blue",
                        genomewideline_color = "red",
                        highlight = NULL,
                        showlegend = TRUE,
                        showgrid = TRUE,
                        xlab = NULL,
                        ylab = "-log10(p)",
                        title = "Manhattan Plot", ...) {

  UseMethod("manhattanly")

}

#' @export
manhattanly.default <- function(x,
                                # col = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(nchr),
                                col = RColorBrewer::brewer.pal(n = 9, name = "Set1"),
                                labelChr = NULL,
                                suggestiveline = -log10(1e-5),
                                genomewideline = -log10(5e-8),
                                suggestiveline_color = "blue",
                                genomewideline_color = "red",
                                highlight = NULL,
                                showlegend = TRUE,
                                showgrid = TRUE,
                                xlab = NULL,
                                ylab = "-log10(p)",
                                title = "Manhattan Plot", ...) {

  mh <- manhattanr(x, ...)
  nchr <- mh$nchr
  manhattanly.manhattanr(mh,
                         col = col,
                         labelChr = labelChr,
                         suggestiveline = suggestiveline,
                         genomewideline = genomewideline,
                         suggestiveline_color = suggestiveline_color,
                         genomewideline_color = genomewideline_color,
                         highlight = highlight,
                         showlegend = showlegend,
                         showgrid = showgrid,
                         xlab = xlab,
                         ylab = ylab,
                         title = title)
}


#' @export
manhattanly.manhattanr <- function(x,
                                   # col = colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Set1"))(nchr),
                                   col = RColorBrewer::brewer.pal(n = 9, name = "Set1"),
                                   labelChr = NULL,
                                   suggestiveline = -log10(1e-5),
                                   genomewideline = -log10(5e-8),
                                   suggestiveline_color = "blue",
                                   genomewideline_color = "red",
                                   highlight = NULL,
                                   highlight_color = "green",
                                   showlegend = TRUE,
                                   showgrid = TRUE,
                                   xlab = NULL,
                                   ylab = "-log10(p)",
                                   title = "Manhattan Plot",
                                   ...) {

  # x <- manhattanr(gwasResults)
  # x <- manhattanr(kk, annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")
  # x <- manhattanr(kk, annotation1 = "ZSCORE")
  # x <- manhattanr(kk, annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")
  #
  #
  # x$data %>% head
  # labelChr <- NULL
  # col <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="Set1")))(22)
  # showgrid <- TRUE
  # labelChr = NULL
  # suggestiveline = -log10(1e-5)
  # genomewideline = -log10(5e-8)
  # suggestiveline_color = "blue"
  # genomewideline_color = "red"
  # highlight_color = "green"
  # highlight = NULL
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

  p <- plot_ly()

  # Add an axis.
  if (nchr == 1) {
    #If single chromosome, ticks and labels automatic.
    p %<>% layout(p,
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
    p %<>% layout(p,
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

    p %<>% add_trace(x = d$pos, y = d$logp,
                     type = "scatter",
                     mode = "markers",
                     evaluate = TRUE,
                     text = TEXT,
                     showlegend = showlegend,
                     marker = list(color = col[1]),
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
      p %<>% add_trace(x = tmp$pos, y = tmp$logp, type = "scatter",
                       mode = "markers", evaluate = TRUE,
                       text = TEXT,
                       showlegend = showlegend,
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
  if (!is.na(snpName)) {
    if (!is.null(highlight)) {
      if (any(!(highlight %in% d[[snpName]]))) warning("You're trying to highlight SNPs that don't exist in your results.")

      d.highlight <- d[which(d[[snpName]] %in% highlight), ]

      p %<>% add_trace(p, x = d.highlight$pos,
                       y = d.highlight$logp,
                       type = "scatter",
                       mode = "markers",
                       evaluate = TRUE,
                       text = d.highlight[[snpName]],
                       showlegend = showlegend,
                       marker = list(color = highlight_color),
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
