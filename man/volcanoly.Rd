% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volcanoly.R
\name{volcanoly}
\alias{volcanoly}
\alias{volcanoly.default}
\alias{volcanoly.volcanor}
\title{Creates a plotly volcano plot}
\usage{
volcanoly(
  x,
  ...,
  col = c("#252525"),
  point_size = 5,
  effect_size_line = c(-1, 1),
  effect_size_line_color = "grey",
  effect_size_line_width = 0.5,
  effect_size_line_type = "dash",
  genomewideline = -log10(1e-05),
  genomewideline_color = "grey",
  genomewideline_width = 0.5,
  genomewideline_type = "dash",
  highlight = NULL,
  highlight_color = "red",
  xlab = NULL,
  ylab = "-log10(p)",
  title = "Volcano Plot"
)
}
\arguments{
\item{x}{Can be an object of class \code{volcanor} produced by the
\code{\link{volcanor}} function or a \code{data.frame} which must contain
at least the following two columns: \itemize{ \item{a p-value, must be
numeric} \item{a measure of the strength of association, typically an odds
ratio, regression coefficient or log fold change. Must be numeric} }}

\item{...}{other parameters passed to \code{\link{volcanor}}}

\item{col}{A character of length 1 indicating the color of the points. Only
the first argument will be used if more than one color is supplied. Can be
\href{https://www.rapidtables.com/web/color/RGB_Color.html}{Hex Codes} as
well.}

\item{point_size}{A \code{numeric} indicating the size of the points on the
plot. Default is 5}

\item{effect_size_line}{Where to draw a "suggestive" line on the x-axis.
Default is \code{-1 and +1}. Must be a vector of length 2. If a longer
vector is supplied, only the first two elements will be used. First element
must be smaller than second element. Set to \code{FALSE} to disable.}

\item{effect_size_line_color}{color of "suggestive" line. Only used if
\code{effect_size_line} is not set to \code{FALSE}. Default is
\code{"blue"}.}

\item{effect_size_line_width}{Width of \code{effect_size_line}. Default is 1.}

\item{effect_size_line_type}{Sets the line type of the
\code{effect_size_line}. Set to a dash type character among "solid", "dot",
"dash", "longdash", "dashdot", or "longdashdot", or a dash length list in
px (eg "5px","10px","2px"). Can also be a positive numeric value (e.g 5,
10, 2). Default is "dash". See
\href{https://plotly.com/r/reference/#layout-shapes}{plotly help page on
layouts} for complete list and more details}

\item{genomewideline}{Where to draw a "genome-wide sigificant" line. Default
\code{-log10(1e-5)}. Set to \code{FALSE} to disable. If more than one
element is provided, only the first will be used}

\item{genomewideline_color}{color of "genome-wide sigificant" line. Only used
if \code{genomewideline} is not set to \code{FALSE}. Default is
\code{"red"}.}

\item{genomewideline_width}{Width of \code{genomewideline}. Default is 1.}

\item{genomewideline_type}{Sets the line type of the \code{genomewideline}.
Set to a dash type character among "solid", "dot", "dash", "longdash",
"dashdot", or "longdashdot", or a dash length list in px (eg
"5px","10px","2px"). Can also be a positive numeric value (e.g 5, 10, 2).
Default is "dash". See
\href{https://plotly.com/r/reference/#layout-shapes}{plotly help page on
layouts} for complete list and more details}

\item{highlight}{A character vector of SNPs in your dataset to highlight.
These SNPs should all be in your dataset. Default is \code{NULL} which
means that all points that are both beyond \code{genomewideline} and
\code{effect_size_line} are highlighted. Set to \code{FALSE} if you don't
want any points highlighted.}

\item{highlight_color}{Color used to highlight points. Only used if
\code{highlight} argument has been specified}

\item{xlab}{X-axis label. Default is \code{NULL} which means that the label
is automatically determined by the \code{\link{volcanor}} function. Specify
here to overwrite the default.}

\item{ylab}{Y-axis label. Default is \code{"-log10(p)"}.}

\item{title}{Title of the plot. Default is \code{"Volcano Plot"}}
}
\value{
An interactive volcano plot.
}
\description{
Creates an interactive volcano plot with multiple annotation options
}
\note{
This package provides additional annotation options and builds on the
  \code{\link{plotly}} \code{d3.js} engine. These plots can be included in
  Shiny apps, Dash apps, Rmarkdown documents or embeded in websites using
  simple HTML code.
}
\examples{
volcanorObj <- volcanor(HapMap,
  p = "P",
  effect_size = "EFFECTSIZE",
  snp = "SNP",
  gene = "GENE"
)
class(volcanorObj)
head(volcanorObj$data)
}
\seealso{
\code{\link{volcanor}}, \code{\link{HapMap}},
  \code{\link{significantSNP}}
}
