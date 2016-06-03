######################################
# R Source code file for creating dataset to be included in the package
# genomic coordinates and RS numbers are taken from hapmap:
# ftp://ftp.ncbi.nlm.nih.gov/hapmap/genotypes/2009-01_phaseIII/plink_format/
# http://hgdownload.cse.ucsc.edu/goldenPath/hg18/database/
# p-values, zscore, effect sizes were simulated randomly
# Author: Sahir Bhatnagar
# Created: June 1st 2016
# Updated:
# Notes: Used bedtools and Vince Forgetta's skills to get annotations
# from UCSC genome browser
#####################################

library(data.table)
rm(list = ls())
# library(readr)
# ftp://ftp.ncbi.nlm.nih.gov/hapmap/genotypes/2009-01_phaseIII/plink_format/
# DT <- data.table::fread("~/Downloads/hapmap3_r2_b36_fwd.consensus.qc.poly.map")
DT <- data.table::fread("../hapmap3_r2_b36_fwd.consensus.qc.poly.map")
DT[, V3:=NULL]
setnames(DT, c("CHR","SNP","BP"))
setkey(DT, CHR)
DT[, chr:=paste0("chr",CHR)]

tmp <- DT[, .N, by = list(CHR)]
tmp[, num:=ceiling(N*0.025)]
tmp[, sum(num)]

devtools::source_gist("https://gist.github.com/mrdwab/933ffeaa7a1d718bd10a")

# include X chromosome
size <- tmp$num[1:23]
names(size) <- paste0("chr",c(1:23))

set.seed(12345)
rm(hapmap)
hapmap <- stratifiedDT(DT,"chr", size = size, select = list(chr = paste0("chr",c(1:23))))
# hapmap[, table(chr)]
# hapmap[, `:=`(P = runif(.N), ZSCORE = rnorm(.N, sd = 0.3), EFFECTSIZE = rnorm(.N, sd = 0.1))]
hapmap[, `:=`(P = runif(.N), EFFECTSIZE = rnorm(.N, sd = 0.1))]
# hapmap[, hist(P)]
# hapmap[, hist(ZSCORE)]
# hapmap[, hist(EFFECTSIZE)]

significantRS1 <- hapmap[CHR %in% 5][order(BP)][2000:2010]$SNP
significantRS2 <- hapmap[CHR %in% 5][order(BP)][1990:1999]$SNP
significantRS3 <- hapmap[CHR %in% 5][order(BP)][1980:1989]$SNP
significantRS4 <- hapmap[CHR %in% 5][order(BP)][1970:1979]$SNP
significantRS5 <- hapmap[CHR %in% 5][order(BP)][1960:1969]$SNP

# significantRS <- hapmap[CHR %in% c(5)][sample(.N, 100)]$SNP
# hapmap[SNP %in% significantRS1, `:=`(P = sample(seq(1e-7, 5e-5, length.out = 2e2),
#                                           size = .N, replace = F),
#                                     ZSCORE = rnorm(.N, sd = 0.5),
#                                     EFFECTSIZE = rnorm(.N, sd = 0.3))]
# hapmap[SNP %in% significantRS2, `:=`(P = sample(seq(1e-10, 5e-3, length.out = 2e2),
#                                           size = .N, replace = F),
#                                      ZSCORE = rnorm(.N, sd = 0.6),
#                                      EFFECTSIZE = rnorm(.N, sd = 0.8))]
# hapmap[SNP %in% significantRS3, `:=`(P = sample(seq(1e-14, 1e-8, by = 9e-12),
#                                            size = .N, replace = F),
#                                      ZSCORE = rnorm(.N, sd = 0.8),
#                                      EFFECTSIZE = rnorm(.N, sd = 1))]
# hapmap[SNP %in% significantRS4, `:=`(P = sample(seq(1e-8, 1e-6, length.out = 2e2),
#                                            size = .N, replace = F),
#                                      ZSCORE = rnorm(.N, sd = 0.6),
#                                      EFFECTSIZE = rnorm(.N, sd = 0.8))]
# hapmap[SNP %in% significantRS5, `:=`(P = sample(seq(1e-6, 5e-4, length.out = 2e2),
#                                            size = .N, replace = F),
#                                      ZSCORE = rnorm(.N, sd = 0.5),
#                                      EFFECTSIZE = rnorm(.N, sd = 0.3))]

hapmap[SNP %in% significantRS1, `:=`(P = sample(seq(1e-7, 5e-5, length.out = 2e2),
                                                size = .N, replace = F),
                                     EFFECTSIZE = rnorm(.N, sd = 0.3))]
hapmap[SNP %in% significantRS2, `:=`(P = sample(seq(1e-10, 5e-3, length.out = 2e2),
                                                size = .N, replace = F),
                                     EFFECTSIZE = rnorm(.N, sd = 0.8))]
hapmap[SNP %in% significantRS3, `:=`(P = sample(seq(1e-14, 1e-8, by = 9e-12),
                                                size = .N, replace = F),
                                     EFFECTSIZE = rnorm(.N, sd = 1))]
hapmap[SNP %in% significantRS4, `:=`(P = sample(seq(1e-8, 1e-6, length.out = 2e2),
                                                size = .N, replace = F),
                                     EFFECTSIZE = rnorm(.N, sd = 0.8))]
hapmap[SNP %in% significantRS5, `:=`(P = sample(seq(1e-6, 5e-4, length.out = 2e2),
                                                size = .N, replace = F),
                                     EFFECTSIZE = rnorm(.N, sd = 0.3))]


hapmap[, `:=`(ZSCORE = round(qnorm(P/2, lower.tail = FALSE), 4), EFFECTSIZE = round(EFFECTSIZE, 4))]


# read in other annotation information
# http://hgdownload.cse.ucsc.edu/goldenPath/hg18/database/
# DTannot <- data.table::fread("~/Downloads/hapmap3_r2_b36_fwd.consensus.qc.poly.kg.out.gene")
DTannot <- data.table::fread("../hapmap3_r2_b36_fwd.consensus.qc.poly.kg.out.gene")
setkey(DTannot, V4)
setkey(hapmap, SNP)

hapmap <- unique(DTannot)[hapmap][, c("CHR", "BP", "P", "V4", "ZSCORE", "EFFECTSIZE", "V12","V11"), with = F]
setnames(hapmap, c("V4", "V12","V11"), c("SNP", "GENE", "DISTANCE"))
setkey(hapmap, CHR, BP)

str(hapmap)

significantSNP <- hapmap[P<1e-6]$SNP
hapmap <- as.data.frame(hapmap)

devtools::use_data(hapmap, overwrite = TRUE)
# write_csv(hapmap, "data-raw/hapmap.csv")
# save(hapmap, file = "data/hapmap.rda")

# devtools::use_data(hapmap, overwrite = TRUE)
devtools::use_data(significantSNP, overwrite = TRUE)

# library(pcev)
# library(survival)
# devtools::use_data(veteran, overwrite = TRUE)


tools::checkRdaFiles("data/")
tools::resaveRdaFiles("data/")

help(hapmap)
devtools::check()
library(devtools)
document()
head(hapmap)
devtools::build_vignettes()
devtools::build()


# devtools::load_all()
manhattanly(hapmap)
manhattanly(hapmap, p = "ZSCORE", snp = "SNP", annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")


pp <- manhattanly(kk, snp = "SNP", gene = "GENE", annotation1 = "EFFECTSIZE",
                  xlab = "hello", ylab = "my nam is", title = "all day",
                  genomewideline = FALSE, suggestiveline = FALSE, showlegend = F,
                  showgrid = F)

pp %>% layout(annotations = list(
  list(x = 0.2, y = 1,
       xref = "paper", yref = "paper",
       xanchor = "left", yanchor = "top",
       ax = 0, ay = 0,
       text = "Visualizing <em>boot.stepAIC()</em>",
       font = list(family = "serif", size = 30)),

  list(x = 0.8, y = 0.90,
       xref = "paper", yref = "paper",
       xanchor = "left", yanchor = "top", align = "left",
       ax = 0, ay = 0,
       text = paste0("<a href='http://www.w3schools.com'>Visit W3Schools.com!</a>", 43, "<br>",
                     "<em>No. of bootstrap samples:</em>", 57, "<br>"),
       font = list(family = "PT Sans Narrow", size = 15))
))


manhattanly(kk, snp = "SNP", gene = "GENE", annotation1 = "EFFECTSIZE",
            xlab = "hello", ylab = "my nam is", title = "all day",
            genomewideline = FALSE, suggestiveline = FALSE, showlegend = F,
            showgrid = F)






