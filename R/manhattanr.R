#' Creates a manhattanr object
#'
#' An object of class manhattanr includes all the needed information for
#' producing a manhattan plot. In the same spirit as the \link{heatmaply}
#' package, the goal is to seperate the pre-processing of the manhattan plot
#' elements from the graphical rendaring of the object, which could be done
#' using any graphical device including \code{\link[plotly]{plot_ly}} and
#' \code{\link[graphics]{plot}} in base \code{R}.
#'
#' @param x A data.frame with columns "BP," "CHR," "P," and optionally, "SNP."
#' @param chr A string denoting the column name for the chromosome. Defaults to
#'   PLINK's "CHR." Said column must be numeric. If you have X, Y, or MT
#'   chromosomes, be sure to renumber these 23, 24, 25, etc.
#' @param bp A string denoting the column name for the chromosomal position.
#'   Defaults to PLINK's "BP." Said column must be numeric.
#' @param p A string denoting the column name for the p-value. Defaults to
#'   PLINK's "P." Said column must be numeric.
#' @param snp A string denoting the column name for the SNP name (rs number).
#'   Defaults to PLINK's "SNP." Said column should be a character. This
#'   argument is optional
#' @param gene A string denoting the column name for the GENE name. Defaults to
#'   PLINK's "SNP." Said column should be a character. This argument is optional
#' @param logp If TRUE, the -log10 of the p-value is plotted. It isn't very
#'   useful to plot raw p-values, but plotting the raw value could be useful for
#'   other genome-wide plots, for example, peak heights, bayes factors, test
#'   statistics, other "scores," etc.
#' @param ... currently ignored
#'
#' @export
#' @source The pre-processing is mostly the same as the
#'   \code{\link[qqman]{manhattan}} function from the
#'   \href{https://github.com/stephenturner/qqman}{\code{qqman}} package by
#'   \href{http://www.gettinggeneticsdone.com/}{Stephen Turner}
#'
#' @seealso \code{\link[qqman]{manhattan}},
#'   \url{https://github.com/stephenturner/qqman},
#'    \href{https://github.com/nstrayer/D3ManhattanPlots}{D3ManhattanPlots}
#'
manhattanr <- function(x,
                       chr = "CHR",
                       bp = "BP",
                       p = "P",
                       snp,
                       gene,
                       annotation1,
                       annotation2,
                       logp = TRUE,
                       ...) {

  # NULLing out strategy
  # http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  CHR = BP = P = index = NULL
  # dotargs <- list(...)
  # print(dotargs)
  # message(paste(chr, bp, p,snp, gene, dotargs))

  # Check for sensible dataset
  ## Make sure you have chr, bp and p columns.
  if (!(chr %in% names(x))) stop(paste("Column", chr, "not found in 'x' data.frame"))
  if (!(bp %in% names(x))) stop(paste("Column", bp, "not found in 'x' data.frame"))
  if (!(p %in% names(x))) stop(paste("Column", p, "not found 'x' data.frame"))

  ## warn if you don't have a snp column
  if (!missing(snp)) {
    if (!(snp %in% names(x))) stop(sprintf("snp argument specified as %s but this column not found in 'x' data.frame", snp))
  }

  if (!missing(gene)) {
    if(!(gene %in% names(x))) stop(sprintf("gene argument specified as %s but this column not found in 'x' data.frame", gene))
  }

  if (!missing(annotation1)) {
    if (!(annotation1 %in% names(x))) stop(sprintf("annotation1 argument specified as %s but this column not found in 'x' data.frame", annotation1))
  }

  if (!missing(annotation2)) {
    if (!(annotation2 %in% names(x))) stop(sprintf("annotation2 argument specified as %s but this column not found in 'x' data.frame", annotation2))
  }

  # if (!(gene %in% names(x))) warning(paste("No GENE column found. OK unless you're trying to annotate."))

  ## make sure chr, bp, and p columns are numeric.
  if (!is.numeric(x[[chr]])) stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
  if (!is.numeric(x[[bp]])) stop(paste(bp, "column should be numeric."))
  if (!is.numeric(x[[p]])) stop(paste(p, "column should be numeric."))

  # Create a new data.frame with columns called CHR, BP, and P.
  d <- data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[p]])

  # If the input data frame has a SNP column, add it to the new data frame
  # you're creating. Rename columns according to input
  if (!missing(snp)) {
    d <- transform(d, SNP = x[[snp]])
    colnames(d)[which(colnames(d) == "SNP")] <- snp
  }

  if (!missing(gene)) {
    d <- transform(d, GENE = x[[gene]])
    colnames(d)[which(colnames(d) == "GENE")] <- gene
  }

  if (!missing(annotation1)) {
    d <- transform(d, ANNOTATION1 = x[[annotation1]])
    colnames(d)[which(colnames(d) == "ANNOTATION1")] <- annotation1
  }

  if (!missing(annotation2)) {
    d <- transform(d, ANNOTATION2 = x[[annotation2]])
    colnames(d)[which(colnames(d) == "ANNOTATION2")] <- annotation2
  }

  # Set positions, ticks, and labels for plotting
  ## Sort and keep only values where is numeric.
  d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
  d <- d[order(d$CHR, d$BP), ]

  if (logp) {
    d$logp <- -log10(d$P)
  } else {
    d$logp <- d$P
  }

  d$pos <- NA

  # Fixes the bug where one chromosome is missing by adding a sequential index column.
  d$index <- NA
  ind <- 0
  for (i in unique(d$CHR)) {
    ind <- ind + 1
    d[d$CHR==i,]$index <- ind
  }

  # This section sets up positions and ticks. Ticks should be placed in the
  # middle of a chromosome. The a new pos column is added that keeps a running
  # sum of the positions of each successive chromsome. For example:
  # chr bp pos
  # 1   1  1
  # 1   2  2
  # 2   1  3
  # 2   2  4
  # 3   1  5
  nchr <- length(unique(d$CHR))
  if (nchr==1) {
    ## For a single chromosome
    d$pos <- d$BP
    ticks <- floor(length(d$pos))/2+1
    xlabel <- paste('Chromosome',unique(d$CHR),'position')
    labs <- ticks
  } else {
    ## For multiple chromosomes
    lastbase <- 0
    ticks <- NULL
    for (i in unique(d$index)) {
      if (i==1) {
        d[d$index==i, ]$pos <- d[d$index==i, ]$BP
      } else {
        lastbase <- lastbase + tail(subset(d,index==i-1)$BP, 1)
        d[d$index==i, ]$pos <- d[d$index==i, ]$BP + lastbase
      }
      # Old way: assumes SNPs evenly distributed
      # ticks=c(ticks, d[d$index==i, ]$pos[floor(length(d[d$index==i, ]$pos)/2)+1])
      # New way: doesn't make that assumption
      ticks = c(ticks, (min(d[d$index == i,]$pos) + max(d[d$index == i,]$pos))/2 + 1)
    }
    xlabel = 'Chromosome'
    labs <- unique(d$CHR)
  }

  manhattanr <- list(data = d, xlabel = xlabel, ticks = ticks, labs = labs,
                     nchr = nchr, pName = p,
                     snpName = if (missing(snp)) NA else snp,
                     geneName = if (missing(gene)) NA else gene,
                     annotation1Name = if (missing(annotation1)) NA else annotation1,
                     annotation2Name = if (missing(annotation2)) NA else annotation2)

  class(manhattanr) <- "manhattanr"

  manhattanr

}
