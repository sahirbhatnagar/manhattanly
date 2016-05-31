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


library(data.table)
DT <- data.table::fread("~/Downloads/hapmap3_r2_b36_fwd.consensus.qc.poly.map")
DT[, V3:=NULL]
setnames(DT, c("CHR","SNP","BP"))
setkey(DT, CHR)
DT[, chr:=paste0("chr",CHR)]

tmp <- DT[, .N, by = list(CHR)]
tmp[, num:=ceiling(N*0.025)]
tmp[, sum(num)]

devtools::source_gist("https://gist.github.com/mrdwab/933ffeaa7a1d718bd10a")

size <- tmp$num[1:24]
names(size) <- paste0("chr",c(1:23,25))
hapmap <- stratifiedDT(DT,"chr", size = size, select = list(chr = paste0("chr",c(1:23,25))))
hapmap[, table(chr)]
hapmap[, P:=runif(.N)]

devtools::load_all()
manhattanly(hapmap, snp = "SNP")





# first <- DT[unique(DT), , mult = "first", which = TRUE]
# last <- first+tmp$num
#
# gwas <- DT[c(first[1]:last[1],
#              first[2]:last[2],
#              first[3]:last[3],
#              first[4]:last[4],
#              first[5]:last[5],
#              first[6]:last[6],
#              first[7]:last[7],
#              first[8]:last[8],
#              first[9]:last[9],
#              first[10]:last[10],
#              first[11]:last[11],
#              first[12]:last[12],
#              first[13]:last[13],
#              first[14]:last[14],
#              first[15]:last[15],
#              first[16]:last[16],
#              first[17]:last[17],
#              first[18]:last[18],
#              first[19]:last[19],
#              first[20]:last[20],
#              first[21]:last[21],
#              first[22]:last[22],
#              first[23]:last[23],
#              first[24]:last[24]
#              )]

# gwas[, V3:=NULL]
# setnames(gwas, c("CHR","SNP","BP"))
# gwas[,table(CHR)]
# gwas[, P:=runif(.N)]
# gwas[CHR==5][sample(50)]#, P:=runif(50, 1e-10,1e-8)]
# gwas[CHR==5][sample(50)][, P:=runif(50, 1e-10,1e-8)]
# gwas[CHR==5][P<1e-5]
# gwas[P<1e-6]
# runif(50, 1e-10,1e-8)

