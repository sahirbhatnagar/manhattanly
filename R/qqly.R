qqly <- function(x,
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

  UseMethod("qqly")

}

#' @export
qqly.default <- function(x,
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

  qq <- qqr(x, ...)
  qqly.qqr(qq,
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
qqly.qqr <- function(x,
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

  x <- qqr(HapMap, snp = "SNP")

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

  plot_ly(data = d, x = EXPECTED, y = OBSERVED,
          text = TEXT, mode = "markers")


  #     # The old way
  #     plot(e, o, pch=20,
  #          xlab=expression(Expected~~-log[10](italic(p))),
  #          ylab=expression(Observed~~-log[10](italic(p))),
  #          ...)

  # The new way to initialize the plot.
  ## See http://stackoverflow.com/q/23922130/654296
  ## First, define your default arguments
  def_args <- list(pch=20, xlim=c(0, max(e)), ylim=c(0, max(o)),
                   xlab=expression(Expected~~-log[10](italic(p))),
                   ylab=expression(Observed~~-log[10](italic(p)))
  )
  ## Next, get a list of ... arguments
  #dotargs <- as.list(match.call())[-1L]
  dotargs <- list(...)
  ## And call the plot function passing NA, your ... arguments, and the default
  ## arguments that were not defined in the ... arguments.
  tryCatch(do.call("plot", c(list(x=e, y=o), def_args[!names(def_args) %in% names(dotargs)], dotargs)), warn=stop)

  # Add diagonal
  abline(0,1,col="red")


}
