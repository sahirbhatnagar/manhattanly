#' Creates a volcano object
#'
#' An object of class volcano includes all the needed information for producing
#' a volcano plot of p-values against effect sizes or fold-changes. The goal is
#' to seperate the pre-processing of the volcano plot elements from the
#' graphical rendaring of the object, which could be done using any graphical
#' device including \code{\link[plotly]{plot_ly}} and
#' \code{\link[graphics]{plot}} in base \code{R}.
#'
#' @param x A \code{data.frame} which must contain at least the following
#'   columns: \itemize{ \item{a p-value, must be numeric} \item{a measure of the
#'   strength of association, typically an odds ratio, regression coefficient or
#'   log fold change. Must be numeric} }
#' @param p A chracter string denoting the column name for the p-values. Default
#'   is \code{p = "P"}. This column must be \code{numeric} or \code{integer}.
#'   Should not have missing, NA, NaN, or NULL values and should be between 0
#'   and 1.
#' @param effect_size A string denoting the column name for the effect size.
#'   Default is \code{effect_size = "EFFECTSIZE"}. This column must be \code{numeric} or
#'   \code{integer}. Should not have missing, NA, NaN, or NULL values.
#' @param snp A string denoting the column name for the SNP names (e.g. rs
#'   number). This argument is optional but required if you want to highlight
#'   any points. More generally, this column could be anything that identifies
#'   each point being plotted. For example, in an Epigenomewide association
#'   study (EWAS) this could be the probe name or cg number. This column should
#'   contain \code{characters}. This argument is necessary.
#'   \code{\link{volcanoly}} function
#' @inheritParams manhattanr
#' @param ... currently ignored
#' @note This function will return an error if any of the p-values are NA, less
#'   than 0 or greater than 1
#' @return An list object of class \code{volcanor} with the following elements
#'   \describe{  \item{data}{processed data to be used for plotting the volcano
#'   plot including the observed and expected p-values on the -log10 scale}
#'   \item{pName, snpName, geneName, annotation1Name, annotation2Name}{The names
#'   of the columns corresponding to the data provided. This information is used
#'   for annotating the plot in the \code{\link{volcanoly}} function } }
#'
#' @seealso \code{\link{volcanoly}}
#'
#'
#' @examples
#' library(manhattanly)
#' volcanorObj <- volcanor(HapMap)
#' class(volcanorObj)
#' head(volcanorObj)
#' @export

volcanor <- function(x,
                     p = "P",
                     effect_size = "EFFECTSIZE",
                     snp,
                     gene,
                     annotation1,
                     annotation2,
                     ...) {

  # Make sure you have p column exists in x.
  if (!(p %in% names(x))) stop(paste("Column", p, "not found in 'x' data.frame"))
  if (!(effect_size %in% names(x))) stop(paste("Column", effect_size, "not found in 'x' data.frame"))

  # Check for numeric p
  if (!is.numeric(x[[p]])) stop(sprintf("p argument specified as %s but this column is not numeric in the 'x' data.frame", p))
  if (!is.numeric(x[[effect_size]])) stop(sprintf("effect_size argument specified as %s but this column is not numeric in the 'x' data.frame", effect_size))

  # Check if any p are not in (0,1)
  if (any(x[[p]] < 0, na.rm = TRUE)) stop("Negative p-values found. These must be removed.")
  if (any(x[[p]] > 1, na.rm = TRUE)) stop("P-values greater than 1 found. These must be removed.")
  if (any(is.na(x[[p]]))) stop("NA P-values found. These must be removed")

  # check if all specified annotations are in 'x' data.frame
  if (!missing(snp)) {
    if (!(snp %in% names(x))) stop(sprintf("snp argument specified as %s but this column not found in 'x' data.frame", snp))
  }

  if (!missing(gene)) {
    if (!(gene %in% names(x))) stop(sprintf("gene argument specified as %s but this column not found in 'x' data.frame", gene))
  }

  if (!missing(annotation1)) {
    if (!(annotation1 %in% names(x))) stop(sprintf("annotation1 argument specified as %s but this column not found in 'x' data.frame", annotation1))
  }

  if (!missing(annotation2)) {
    if (!(annotation2 %in% names(x))) stop(sprintf("annotation2 argument specified as %s but this column not found in 'x' data.frame", annotation2))
  }

  # Create a new data.frame with columns called P.
  d <- data.frame(EFFECTSIZE = x[[effect_size]], P = x[[p]])

  # If the input data frame has a SNP column, add it to the new data frame
  # you're creating. Rename columns according to input
  if (!missing(snp)) {
    d[["SNP"]] <- x[[snp]]
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

  # Observed and expected
  d[["LOG10P"]] <- -log10(d[["P"]])

  volcanor <- list(
    data = d,
    pName = p,
    effectName = effect_size,
    xlabel = effect_size,
    snpName = if (missing(snp)) NA else snp,
    geneName = if (missing(gene)) NA else gene,
    annotation1Name = if (missing(annotation1)) NA else annotation1,
    annotation2Name = if (missing(annotation2)) NA else annotation2
  )

  class(volcanor) <- "volcanor"

  volcanor
}
