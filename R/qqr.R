#' Creates a qq object
#'
#' An object of class qq includes all the needed information for producing a
#' quantile-quantile plot of p-values. The goal is to seperate the
#' pre-processing of the quantile-quantile plot elements from the graphical
#' rendaring of the object, which could be done using any graphical device
#' including \code{\link[plotly]{plot_ly}} and \code{\link[graphics]{plot}} in
#' base \code{R}.
#'
#' @param x A \code{data.frame} which must contain at least the following
#'   column: \itemize{ \item{a p-value, must be numeric} }
#' @param p A string denoting the column name for the p-values. Default is
#'   \code{p = "P"}. This column must be \code{numeric} or \code{integer}.
#'   Should not have missing, NA, NaN, or NULL values and should be between 0
#'   and 1.
#' @param snp A string denoting the column name for the SNP names (e.g. rs
#'   number). More generally, this column could be anything that identifies each
#'   point being plotted. For example, in an Epigenomewide association study
#'   (EWAS) this could be the probe name or cg number. This column should be a
#'   \code{character}. This argument is optional, however it is necessary to
#'   specify if you want to highlight points on the plot using the
#'   \code{highlight} argument in the \code{\link{qqly}} function
#' @inheritParams manhattanr
#' @param ... currently ignored
#' @note This function will return an error if any of the p-values are NA, less
#'   than 0 or greater than 1
#' @return An list object of class \code{qqr} with the following elements
#'   \describe{  \item{data}{processed data to be used for plotting the Q-Q plot
#'   including the observed and expected p-values on the -log10 scale}
#'   \item{pName, snpName, geneName, annotation1Name, annotation2Name}{The names
#'   of the columns corresponding to the data provided. This information is used
#'   for annotating the plot in the \code{\link{qqly}} function } }
#'
#' @source The calculation of the expected p-value is taken from the
#'   \code{\link[qqman]{qq}} function from the
#'   \href{https://github.com/stephenturner/qqman}{\code{qqman}} package by
#'   \href{http://www.gettinggeneticsdone.com/}{Stephen Turner}
#'
#' @examples
#' \dontrun{
#' library(manhattanly)
#' data(HapMap)
#' qqrObj <- qqr(HapMap, snp = "SNP", highlight = significantSNP)
#' class(qqrObj)
#' head(qqrObj[["data"]])
#' }
#' @export

qqr <- function(x,
                p = "P",
                snp,
                gene,
                annotation1,
                annotation2,
                ...) {

  # x = HapMap
  # p = "P"
  ## Make sure you have p column exists in x.
  if (!(p %in% names(x))) stop(paste("Column", p, "not found 'x' data.frame"))

  # Check for numeric p
  if (!is.numeric(x[[p]])) stop(sprintf("p argument specified as %s but this column is not numeric in the 'x' data.frame", p))

  # Check if any p are not in (0,1)
  if (any(x[[p]]<0)) stop("Negative p-values found. These must be removed.")
  if (any(x[[p]]>1)) stop("P-values greater than 1 found. These must be removed.")
  if (any(is.na(x[[p]]))) stop("NA P-values found. These must be removed")

  ## check if all specified annotations are in 'x' data.frame
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

  # Create a new data.frame with columns called P.
  d <- data.frame(P = x[[p]])

  # If the input data frame has a SNP column, add it to the new data frame
  # you're creating. Rename columns according to input
  if (!missing(snp)) {
    d[["SNP"]] <- x[[snp]]
    # str(d)
    colnames(d)[which(colnames(d) == "SNP")] <- snp
  }

  if (!missing(gene)) {
    d[["GENE"]] <- x[[gene]]
    colnames(d)[which(colnames(d) == "GENE")] <- gene
  }

  if (!missing(annotation1)) {
    d[["ANNOTATION1"]] <- x[[annotation1]]
    colnames(d)[which(colnames(d) == "ANNOTATION1")] <- annotation1
  }

  if (!missing(annotation2)) {
    d[["ANNOTATION2"]] <- x[[annotation2]]
    colnames(d)[which(colnames(d) == "ANNOTATION2")] <- annotation2
  }

  # d <- d[complete.cases(d), ]
  # limit to not missing, not nan, not null, not infinite, between 0 and 1
  # pvector <- pvector[!is.na(pvector) & !is.nan(pvector) & !is.null(pvector) & is.finite(pvector) & pvector<1 & pvector>0]

  # sort d by decreasing p-value
  d <- d[order(d[["P"]],decreasing = FALSE), , drop = FALSE]

  # Observed and expected
  d[["OBSERVED"]] <- -log10(d[["P"]])
  d[["EXPECTED"]] <- -log10(stats::ppoints(length(d[["P"]])))

  qqr <- list(data = d,
              pName = p,
              snpName = if (missing(snp)) NA else snp,
              geneName = if (missing(gene)) NA else gene,
              annotation1Name = if (missing(annotation1)) NA else annotation1,
              annotation2Name = if (missing(annotation2)) NA else annotation2)

  class(qqr) <- "qqr"

  qqr

}
