#' Creates a plotly volcano plot
#'
#' Creates an interactive volcano plot with multiple annotation options
#'
#' @param x Can be an object of class \code{volcanor} produced by the
#'   \code{\link{volcanor}} function or a \code{data.frame} which must contain
#'   at least the following two columns: \itemize{ \item{a p-value, must be
#'   numeric} \item{a measure of the strength of association, typically an odds
#'   ratio, regression coefficient or log fold change. Must be numeric} }
#' @param col A character of length 1 indicating the color of the points. Only
#'   the first argument will be used if more than one color is supplied. Can be
#'   \href{http://www.rapidtables.com/web/color/RGB_Color.htm}{Hex Codes} as
#'   well.
#' @param point_size A \code{numeric} indicating the size of the points on the
#'   plot. Default is 5
#' @param effect_size_line Where to draw a "suggestive" line on the x-axis.
#'   Default is \code{-1 and +1}. Must be a vector of length 2. If a longer
#'   vector is supplied, only the first two elements will be used. First element
#'   must be smaller than second element. Set to \code{FALSE} to disable.
#' @param effect_size_line_color color of "suggestive" line. Only used if
#'   \code{effect_size_line} is not set to \code{FALSE}. Default is
#'   \code{"blue"}.
#' @param effect_size_line_width Width of \code{effect_size_line}. Default is 1.
#' @param effect_size_line_type Sets the line type of the
#'   \code{effect_size_line}. Set to a dash type character among "solid", "dot",
#'   "dash", "longdash", "dashdot", or "longdashdot", or a dash length list in
#'   px (eg "5px","10px","2px"). Can also be a positive numeric value (e.g 5,
#'   10, 2). Default is "dash". See
#'   \href{https://plotly.com/r/reference/#layout-shapes}{plotly help page on
#'   layouts} for complete list and more details
#' @param genomewideline Where to draw a "genome-wide sigificant" line. Default
#'   \code{-log10(1e-5)}. Set to \code{FALSE} to disable. If more than one
#'   element is provided, only the first will be used
#' @param genomewideline_color color of "genome-wide sigificant" line. Only used
#'   if \code{genomewideline} is not set to \code{FALSE}. Default is
#'   \code{"red"}.
#' @param genomewideline_width Width of \code{genomewideline}. Default is 1.
#' @param genomewideline_type Sets the line type of the \code{genomewideline}.
#'   Set to a dash type character among "solid", "dot", "dash", "longdash",
#'   "dashdot", or "longdashdot", or a dash length list in px (eg
#'   "5px","10px","2px"). Can also be a positive numeric value (e.g 5, 10, 2).
#'   Default is "dash". See
#'   \href{https://plotly.com/r/reference/#layout-shapes}{plotly help page on
#'   layouts} for complete list and more details
#' @param highlight A character vector of SNPs in your dataset to highlight.
#'   These SNPs should all be in your dataset. Default is \code{NULL} which
#'   means that all points that are both beyond \code{genomewideline} and
#'   \code{effect_size_line} are highlighted. Set to \code{FALSE} if you don't
#'   want any points highlighted.
#' @param highlight_color Color used to highlight points. Only used if
#'   \code{highlight} argument has been specified
#' @param xlab X-axis label. Default is \code{NULL} which means that the label
#'   is automatically determined by the \code{\link{volcanor}} function. Specify
#'   here to overwrite the default.
#' @param ylab Y-axis label. Default is \code{"-log10(p)"}.
#' @param title Title of the plot. Default is \code{"Volcano Plot"}
#' @param ... other parameters passed to \code{\link{volcanor}}
#' @note This package provides additional annotation options and builds on the
#'   \code{\link{plotly}} \code{d3.js} engine. These plots can be included in
#'   Shiny apps, Dash apps, Rmarkdown documents or embeded in websites using
#'   simple HTML code.
#' @return An interactive volcano plot.
#' @seealso \code{\link{volcanor}}, \code{\link{HapMap}},
#'   \code{\link{significantSNP}}
#' @aliases volcanoly.default volcanoly.volcanor
#' @importFrom magrittr '%<>%'
#' @import plotly
#' @export
#' @examples
#' volcanorObj <- volcanor(HapMap, 
#'   p = "P",
#'   effect_size = "EFFECTSIZE",
#'   snp = "SNP",
#'   gene = "GENE")
#' class(volcanorObj)
#' head(volcanorObj$data)

volcanoly <- function(x,
                      col = c("#252525"),
                      point_size = 5,
                      effect_size_line = c(-1,1),
                      effect_size_line_color = "grey",
                      effect_size_line_width = 0.5,
                      effect_size_line_type = "dash",
                      genomewideline = -log10(1e-5),
                      genomewideline_color = "grey",
                      genomewideline_width = 0.5,
                      genomewideline_type = "dash",
                      highlight = NULL,
                      highlight_color = "red",
                      xlab = NULL,
                      ylab = "-log10(p)",
                      title = "Volcano Plot", ...) {
  
  UseMethod("volcanoly")
  
}

#' @export
volcanoly.default <- function(x,
                              col = c("#252525"),
                              point_size = 5,
                              effect_size_line = c(-1,1),
                              effect_size_line_color = "grey",
                              effect_size_line_width = 0.5,
                              effect_size_line_type = "dash",
                              genomewideline = -log10(1e-5),
                              genomewideline_color = "grey",
                              genomewideline_width = 0.5,
                              genomewideline_type = "dash",
                              highlight = NULL,
                              highlight_color = "red",
                              xlab = NULL,
                              ylab = "-log10(p)",
                              title = "Volcano Plot", ...) {
  
  mh <- volcanor(x, ...)
  volcanoly.volcanor(mh,
                     col = col,
                     point_size = point_size,
                     effect_size_line = effect_size_line,
                     effect_size_line_color = effect_size_line_color,
                     effect_size_line_width = effect_size_line_width,
                     effect_size_line_type = effect_size_line_type,
                     genomewideline = genomewideline,
                     genomewideline_color = genomewideline_color,
                     genomewideline_width = genomewideline_width,
                     genomewideline_type = genomewideline_type,
                     highlight = highlight,
                     highlight_color = highlight_color,
                     xlab = xlab,
                     ylab = ylab,
                     title = title)
}


my_vline <- function(x = 0, color = "grey", dash = "dash", width = 2) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, width = width, dash = dash)
  )
}

my_hline <- function(y = 0, color = "grey", dash = "dash", width = 2) {
  
  # https://plotly.com/r/reference/#layout-shapes for dash arguments
  # can be number (to keep original API or character)
  # dash <- match.arg(dash)
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, width = width, dash = dash)
  )
}


#' @export
volcanoly.volcanor <- function(x,
                               col = c("#252525"),
                               point_size = 5,
                               effect_size_line = c(-1,1),
                               effect_size_line_color = "grey",
                               effect_size_line_width = 0.5,
                               effect_size_line_type = "dash",
                               genomewideline = -log10(1e-5),
                               genomewideline_color = "grey",
                               genomewideline_width = 0.5,
                               genomewideline_type = "dash",
                               highlight = NULL,
                               highlight_color = "red",
                               xlab = NULL,
                               ylab = "-log10(p)",
                               title = "Volcano Plot",
                               ...) {
  
  # x <- volcanor(gwasResults)
  # x <- volcanor(kk, annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")
  # x <- volcanor(kk, annotation1 = "ZSCORE")
  # x <- volcanor(kk, annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")
  # x <- volcanor(HapMap)
  # # #
  # x$data %>% head
  # str(x$data)
  # showgrid <- TRUE
  # point_size = 5
  # effect_size_line = c(-1,1)
  # effect_size_line_type = 2
  # genomewideline_type = 2
  # genomewideline = -log10(1e-5)
  # genomewideline_type = 2
  # effect_size_line_color = "grey"
  # genomewideline_color = "grey"
  # effect_size_line_width = genomewideline_width = 0.5;
  # highlight_color = "red"
  # highlight = NULL
  # ylab = "-log10(p)"
  # xlab = NULL
  # title = "Volcano Plot"
  # col = c("#252525")
  
  #########
  
  d <- x$data
  pName <- x$pName
  log10pName <- "LOG10P"
  effectName <- x$effectName
  snpName <- x$snpName
  geneName <- x$geneName
  annotation1Name <- x$annotation1Name
  annotation2Name <- x$annotation2Name
  labs <- x$labs
  xlabel <- x$xlabel
  # print(effect_size_line)
  
  if (!is.null(highlight) & is.na(snpName)) stop("You're trying to highlight snps, but havent provided a snp column")
  if (!is.logical(effect_size_line)) {
    if (length(effect_size_line) < 2) stop("'effect_size_line' must be a numeric vector of length 2")
    if (length(effect_size_line) > 2) message("More than two values provided to 'effect_size_line'. Only the first two elements will be used")
    if (effect_size_line[1] > effect_size_line[2]) stop("First element of 'effect_size_line' must be smaller than second element")
  }
  if (is.logical(effect_size_line)) {
    if (effect_size_line) stop("If effect_size_line is a logical, it must be set to FALSE")
  }
  if (is.logical(genomewideline)) {
    if (genomewideline) stop("If genomewideline is a logical, it must be set to FALSE")
  }
  if (is.null(highlight) & is.logical(effect_size_line) & is.logical(genomewideline)) 
    message("Since both effect_size_line and genomewideline are set to FALSE, no points will be highlighted")
  
  if (!is.null(highlight) && is.logical(highlight) && highlight) stop("'highlight' argument must be set to either NULL, FALSE, or a character vector of SNPs to highlight")
  # Initialize plot
  # xmax = ceiling(max(d$EFFECTSIZE) * 1.03)
  # xmin = floor(max(d$EFFECTSIZE) * -0.03)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Initalize plotly
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # rm(p)
  
  
  # TEXT <- paste(if (!is.na(snpName)) paste0(snpName,": ",d[[snpName]]),
  #               if (!is.na(geneName)) paste0(geneName,": ",d[[geneName]]),
  #               # if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d[[annotation1Name]]),
  #               # if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d[[annotation2Name]]), 
  #               sep = "<br>")
  
  my_xlab <- list(
    title = if(!is.null(xlab)) xlab else xlabel
  )
  
  my_ylab <- list(
    title = ylab
  )
  
  
  
  
  fig <- plot_ly(d, x = ~EFFECTSIZE, y = ~LOG10P,
                 type = 'scatter', mode = 'markers',
                 hoverinfo = 'text',
                 marker = list(color = col,
                               size = point_size),
                 text = ~paste0(if (!is.na(snpName)) paste0(snpName,": ",d[[snpName]],"<br>"),
                               if (!is.na(geneName)) paste0(geneName,": ",d[[geneName]],"<br>"),
                               if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d[[annotation1Name]],"<br>"),
                               if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d[[annotation2Name]],"<br>"),
                               paste0(effectName,": ",EFFECTSIZE,"<br>"),
                               paste0(log10pName,": ",LOG10P)
                               )
                 ) %>% plotly::layout(xaxis = my_xlab, yaxis = my_ylab, title = list(text = title))
  
  # fig
  # browser()
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Keep going from here
  # p <- ggplot2::ggplot(d, ggplot2::aes(x = EFFECTSIZE, y = LOG10P)) + 
  #   ggplot2::geom_point() + 
  #   ggplot2::theme_classic() + 
  #   ggplot2::labs(x = if(!is.null(xlab)) xlab else xlabel,
  #                 y = ylab,
  #                 title = title)
  
  if (!is.logical(effect_size_line) & !is.logical(genomewideline)) {
    
    p2 <- list(
      my_vline(x = effect_size_line[1], color = effect_size_line_color, dash = effect_size_line_type, width = effect_size_line_width),
      my_vline(x = effect_size_line[2], color = effect_size_line_color, dash = effect_size_line_type, width = effect_size_line_width),
      my_hline(y = genomewideline[1], color = genomewideline_color, dash = genomewideline_type, width = genomewideline_width)
    )
    
    # eline1 <- ggplot2::geom_vline(xintercept = effect_size_line[1], 
    #                               linetype = effect_size_line_type, 
    #                               size = effect_size_line_width, 
    #                               color = effect_size_line_color)
    # eline2 <- ggplot2::geom_vline(xintercept = effect_size_line[2],
    #                               linetype = effect_size_line_type,
    #                               size = effect_size_line_width,
    #                               color = effect_size_line_color)
    # pline <- ggplot2::geom_hline(yintercept = genomewideline[1],
    #                              linetype = genomewideline_type,
    #                              size = genomewideline_width,
    #                              color = genomewideline_color)
    
    # fig <- fig %>% layout(shapes = p2)
    # p <- p + eline1 + eline2 + pline
  }
  
  if (is.logical(effect_size_line) & !is.logical(genomewideline)) {
    
    # pline <- ggplot2::geom_hline(yintercept = genomewideline[1],
    #                              linetype = genomewideline_type,
    #                              size = genomewideline_width,
    #                              color = genomewideline_color)
    
    p2 <- list(
      my_hline(y = genomewideline[1], color = genomewideline_color, dash = genomewideline_type, width = genomewideline_width)
    )
    
    # p <- p + pline
  }
  
  
  if (!is.logical(effect_size_line) & is.logical(genomewideline)) {
    
    
    p2 <- list(
      my_vline(x = effect_size_line[1], color = effect_size_line_color, dash = effect_size_line_type, width = effect_size_line_width),
      my_vline(x = effect_size_line[2], color = effect_size_line_color, dash = effect_size_line_type, width = effect_size_line_width)
      # my_hline(y = genomewideline[1], color = genomewideline_color, dash = genomewideline_type, width = genomewideline_width)
    )
    
    # eline1 <- ggplot2::geom_vline(xintercept = effect_size_line[1], 
    #                               linetype = effect_size_line_type, 
    #                               size = effect_size_line_width, 
    #                               color = effect_size_line_color)
    # eline2 <- ggplot2::geom_vline(xintercept = effect_size_line[2],
    #                               linetype = effect_size_line_type,
    #                               size = effect_size_line_width,
    #                               color = effect_size_line_color)
    # 
    # p <- p + eline1 + eline2
  }
  
  if (is.logical(effect_size_line) & is.logical(genomewideline)) {
    p2 <- list(NULL)
  }
  
  # fig <- 
  p <- fig %>% layout(shapes = p2) 
  
  # p <- plotly::ggplotly(p)
  
  # pp %<>% plotly::add_trace(marker = list(color = col,size = point_size, text = TEXT)) 
  # if (!(is.na(snpName) && is.na(geneName) && is.na(annotation1Name) && is.na(annotation2Name))) {
    # p %<>% plotly::add_trace(
    #   type = "scatter",
    #   mode = "markers",
    #   text = TEXT,
    #   marker = list(color = col, size = point_size)
    #   )
    # 
    # p %>% add_markers()
    # 
    # p %<>% plotly::add_text(
    #   # type = "scatter",
    #   # mode = "markers",
    #   text = TEXT
    #   # marker = list(color = col, size = point_size)
    # )
    
  # }
  
  
  # p %<>%  
  #   plotly::ggplotly() %<>%  
  #   plotly::add_markers(
  #     marker = list(
  #       color = col,
  #       size = point_size,
  #       text = TEXT)) 
  

  # automatic highlighting
  if (is.null(highlight)) {
    if (!is.na(snpName)) {
      
      # Highlight snps automatically to be those greater than genomewideline and effect_size_line
      if ((is.null(highlight) & !is.logical(effect_size_line)) | (is.null(highlight) & !is.logical(genomewideline))) {
        
        # if both lines are provided
        if (!is.logical(effect_size_line) & !is.logical(genomewideline)) {
          
          highlight_index <- c(which((d$EFFECTSIZE < effect_size_line[1]) & (d$LOG10P > genomewideline)), 
                               which((d$EFFECTSIZE > effect_size_line[2]) & (d$LOG10P > genomewideline)))
          
        } else if (!is.logical(effect_size_line) & is.logical(genomewideline)) {
          
          # if only effect_size_line is provided
          highlight_index <- c(which(d$EFFECTSIZE < effect_size_line[1]), 
                               which(d$EFFECTSIZE > effect_size_line[2]))
          
        } else if (is.logical(effect_size_line) & !is.logical(genomewideline)) {
          
          # if only genomewideline is provided
          highlight_index <- which(d$LOG10P > genomewideline)
        }  
        
        if (length(highlight_index)==0) message("No points are beyond the effect_size_line or genomewideline, therefore no points will be highlighted")
        if (length(highlight_index)>0) {
          
          d.highlight <- d[highlight_index, ] 
          
          
          # TEXT <- paste(if (!is.na(snpName)) paste0(snpName,": ",d.highlight[[snpName]]),
          #               if (!is.na(geneName)) paste0(geneName,": ",d.highlight[[geneName]]), sep = "<br>")
          

          p %<>% plotly::add_trace(x = ~EFFECTSIZE, 
                                  y = ~LOG10P,
                                  data = d.highlight, 
                                  inherit = FALSE,
                                  type = "scatter",
                                  mode = "markers",
                                  marker = list(color = highlight_color,
                                                size = point_size),
                                  # name = "of interest",
                                  showlegend = FALSE,
                                  text = ~paste0(if (!is.na(snpName)) paste0(snpName,": ",d.highlight[[snpName]],"<br>"),
                                                if (!is.na(geneName)) paste0(geneName,": ",d.highlight[[geneName]],"<br>"),
                                                if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d.highlight[[annotation1Name]],"<br>"),
                                                if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d.highlight[[annotation2Name]],"<br>"),
                                                paste0(effectName,": ",EFFECTSIZE,"<br>"),
                                                paste0(log10pName,": ",LOG10P))
          )
          
          # p %>% plotly::add_trace(x = d.highlight$EFFECTSIZE, y = d.highlight$LOG10P,
          #                          type = "scatter",
          #                          mode = "markers",
          #                          text = TEXT,
          #                          marker = list(color = highlight_color,
          #                                        size = point_size),
          #                          name = "of interest")
          

        }
        
      }
    }
  }  
      
      # user specified highlighting
      if (!is.null(highlight) && !is.logical(highlight)) {
        if (!is.na(snpName)) {
        
        if (any(!(highlight %in% d[[snpName]]))) warning("You're trying to highlight SNPs that don't exist in your results.")
        
        d.highlight <- d[which(d[[snpName]] %in% highlight), ]
        
        # TEXT <- paste(if (!is.na(snpName)) paste0(snpName,": ",d.highlight[[snpName]]),
        #               if (!is.na(geneName)) paste0(geneName,": ",d.highlight[[geneName]]),
        #               if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d.highlight[[annotation1Name]]),
        #               if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d.highlight[[annotation2Name]]), sep = "<br>")
        
        
        p %<>% plotly::add_trace(x = ~EFFECTSIZE, 
                                 y = ~LOG10P,
                                 data = d.highlight, 
                                 inherit = FALSE,
                                 type = "scatter",
                                 mode = "markers",
                                 marker = list(color = highlight_color,
                                               size = point_size),
                                 # name = "of interest",
                                 showlegend = FALSE,
                                 text = ~paste0(if (!is.na(snpName)) paste0(snpName,": ",d.highlight[[snpName]],"<br>"),
                                                if (!is.na(geneName)) paste0(geneName,": ",d.highlight[[geneName]],"<br>"),
                                                if (!is.na(annotation1Name)) paste0(annotation1Name,": ",d.highlight[[annotation1Name]],"<br>"),
                                                if (!is.na(annotation2Name)) paste0(annotation2Name,": ",d.highlight[[annotation2Name]],"<br>"),
                                                paste0(effectName,": ",EFFECTSIZE,"<br>"),
                                                paste0(log10pName,": ",LOG10P))
        )
        
        # p %<>% plotly::add_trace(x = d.highlight$EFFECTSIZE, y = d.highlight$LOG10P,
        #                          type = "scatter",
        #                          mode = "markers",
        #                          text = TEXT,
        #                          marker = list(color = highlight_color,
        #                                        size = point_size),
        #                          name = "of interest")
        
      }
      
    }
  
  p
}

