library(qqman)
dev.off()
manhattan(gwasResults)
manhattan
rm(list=ls())
View(manhattan)


manhattan_plotly <- function (x, chr = "CHR", bp = "BP", p = "P", 
                              snp = "SNP", col = c("gray10", "gray60"), 
                              chrlabs = NULL, suggestiveline = -log10(1e-05), 
                              genomewideline = -log10(5e-08), highlight = NULL, 
                              logp = TRUE, ...) {
  x = gwasResults ; chr = "CHR" ; bp = "BP" ; p = "P" ; 
  snp = "SNP" ; col = c("gray10", "gray60") ;  
  chrlabs = NULL; suggestiveline = -log10(1e-05); 
  genomewideline = -log10(5e-08); highlight = NULL; 
  logp = TRUE
  
  
  CHR = BP = P = index = NULL
  if (!(chr %in% names(x))) 
    stop(paste("Column", chr, "not found!"))
  if (!(bp %in% names(x))) 
    stop(paste("Column", bp, "not found!"))
  if (!(p %in% names(x))) 
    stop(paste("Column", p, "not found!"))
  if (!(snp %in% names(x))) 
    warning(paste("No SNP column found. OK unless you're trying to highlight."))
  if (!is.numeric(x[[chr]])) 
    stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
  if (!is.numeric(x[[bp]])) 
    stop(paste(bp, "column should be numeric."))
  if (!is.numeric(x[[p]])) 
    stop(paste(p, "column should be numeric."))
  d = data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[p]])
  if (!is.null(x[[snp]])) 
    d = transform(d, SNP = x[[snp]])
  d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
  d <- d[order(d$CHR, d$BP), ]
  if (logp) {
    d$logp <- -log10(d$P)
  } else {
    d$logp <- d$P
  }
  d$pos = NA
  d$index = NA
  ind = 0
  for (i in unique(d$CHR)) {
    ind = ind + 1
    d[d$CHR == i, ]$index = ind
  }
  nchr = length(unique(d$CHR))
  if (nchr == 1) {
    options(scipen = 999)
    d$pos = d$BP/1e+06
    ticks = floor(length(d$pos))/2 + 1
    xlabel = paste("Chromosome", unique(d$CHR), "position(Mb)")
    labs = ticks
  } else {
    lastbase = 0
    ticks = NULL
    for (i in unique(d$index)) {
      if (i == 1) {
        d[d$index == i, ]$pos = d[d$index == i, ]$BP
      } else {
        lastbase = lastbase + tail(subset(d, index == 
                                            i - 1)$BP, 1)
        d[d$index == i, ]$pos = d[d$index == i, ]$BP + 
          lastbase
      }
      ticks = c(ticks, (min(d[d$CHR == i, ]$pos) + max(d[d$CHR == 
                                                           i, ]$pos))/2 + 1)
    }
    xlabel = "Chromosome"
    labs <- unique(d$CHR)
  }
  xmax = ceiling(max(d$pos) * 1.03)
  xmin = floor(max(d$pos) * -0.03)
  def_args <- list(xaxt = "n", bty = "n", xaxs = "i", yaxs = "i", 
                   las = 1, pch = 20, xlim = c(xmin, xmax), 
                   ylim = c(0,ceiling(max(d$logp))), xlab = xlabel, 
                   ylab = expression(-log[10](italic(p))))
  plotly::plot_ly()
  dotargs <- list(...)
  dotargs = NULL
  do.call("plot", c(NA, dotargs, def_args[!names(def_args) %in% 
                                            names(dotargs)]))
  

  if (!is.null(chrlabs)) {
    if (is.character(chrlabs)) {
      if (length(chrlabs) == length(labs)) {
        labs <- chrlabs
      } else {
        warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
      }
    } else {
      warning("If you're trying to specify chromosome labels, chrlabs must be a character vector")
    }
  }
  if (nchr == 1) {
    axis(1, ...)
  }  else {
    axis(1, at = ticks, labels = labs, ...)
  }
  col = rep(col, max(d$CHR))
  if (nchr == 1) {
    with(d, points(pos, logp, pch = 20, col = col[1], ...))
  } else {
    icol = 1
    for (i in unique(d$index)) {
      with(d[d$index == unique(d$index)[i], ], points(pos, 
                                  logp, col = col[icol], pch = 20, ...))
      icol = icol + 1
    }
  }
  
  
  p <- plot_ly()
  p <- layout(p,              # all of layout's properties: /r/reference/#layout
              title = "Manhattan Plot", # layout's title: /r/reference/#layout-title
              xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                title = "Chromosome",     # xaxis's title: /r/reference/#layout-xaxis-title
                showgrid = T,        # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
                range = c(xmin, xmax),
                autotick = FALSE,
                tickmode="array",
                tickvals=ticks,
                ticktext=labs,
                ticks = "outside"
              ),
              yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                title = "-log10(p)",
                range = c(0,ceiling(max(d$logp))) # yaxis's title: /r/reference/#layout-yaxis-title
              )
  )
  p
  

library(magrittr)
  col = rep(1:2, max(d$CHR))
  icol=1
for(i in unique(d$index)) {
    #i=1
    tmp <- d[d$index == unique(d$index)[i], ]
    p %<>% add_trace(x = tmp$pos, y = tmp$logp, type = "scatter", 
                     mode = "markers", evaluate = TRUE,
                     text = paste0(tmp$SNP,tmp$SNP),
                     marker = list(          # marker is a named list, valid keys: /r/reference/#scatter-marker
                       color=col[icol]     # more about marker's "color" attribute: /r/reference/#scatter-marker-color
                     ),
                     name=paste0("chr",i))
    icol = icol + 1
  }

  p  %>%
  layout(showlegend = F) %>%
  add_trace(x = c(xmin, xmax), y= c(genomewideline, genomewideline), mode = "lines",
            color = "black") %>% 
  add_trace(x = c(xmin, xmax), y= c(suggestiveline, suggestiveline), mode = "lines")
  
  
plotly_POST(p, filename = "r-docs/manhattan")



  p <- plot_ly(d,
               mode = "markers",
               type = "scatter",       # all "scatter" attributes: https://plot.ly/r/reference/#scatter
               x = pos,               # more about scatter's "x": /r/reference/#scatter-x
               y = logp,            # more about scatter's "y": /r/reference/#scatter-y
               name = "Manhattan",  # more about scatter's "name": /r/reference/#scatter-name
               marker = list(          # marker is a named list, valid keys: /r/reference/#scatter-marker
                 color="rgb(16, 32, 77)"     # more about marker's "color" attribute: /r/reference/#scatter-marker-color
               ))
  p
  
  
  
  if (suggestiveline) 
    abline(h = suggestiveline, col = "blue")
  if (genomewideline) 
    abline(h = genomewideline, col = "red")
  if (!is.null(highlight)) {
    if (any(!(highlight %in% d$SNP))) 
      warning("You're trying to highlight SNPs that don't exist in your results.")
    d.highlight = d[which(d$SNP %in% highlight), ]
    with(d.highlight, points(pos, logp, col = "green3", 
                             pch = 20, ...))
  }
}
