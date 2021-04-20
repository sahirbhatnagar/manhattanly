data("HapMap")
DT_default <- qqr(HapMap, 
                  p = "P")

#include snp and gene information
DT_snp_gene <- qqr(HapMap, 
                   p = "P",
                   snp = "SNP",
                   gene = "GENE")
# head(DT_snp_gene[["data"]])


# change colnames
HapMap2 <- HapMap
colnames(HapMap2) <- c("chromosome","baseposition","Pval","rs","zvalue", "logFC","taxa","dist")

qqrObj2 <- qqr(HapMap2, 
               p = "Pval",
               snp = "rs",
               gene = "taxa")

DT2_default <- qqr(HapMap2, 
                   p = "Pval")

DT2_snp_gene <- qqr(HapMap2, 
                    p = "Pval",
                    snp = "rs",
                    gene = "taxa")

# add annotations
DT_annotations <- qqr(HapMap2, 
                      p = "Pval",
                      snp = "rs",
                      gene = "taxa",
                      annotation1 = "zvalue",
                      annotation2 = "chromosome")


test_that("qqly on HapMap", {
  skip_on_cran()
  expect_s3_class(qqly(DT_default), "plotly")
  expect_s3_class(qqly(HapMap), "plotly")
  expect_s3_class(qqly(HapMap, snp = "SNP"), "plotly")
  
  expect_s3_class(qqly(DT2_snp_gene, highlight = sample(DT2_snp_gene$data$rs,10), 
                            highlight_color = "green", point_size = 8), "plotly")
  
  expect_s3_class(qqly(HapMap, snp = "SNP", highlight = significantSNP), "plotly")
  expect_s3_class(qqly(DT_annotations, highlight = significantSNP), "plotly")
  expect_s3_class(qqly(subset(HapMap, CHR %in% 4), snp = "SNP", highlight = c("rs7689106", "rs4242017", "rs1980271", "rs13117731", "rs1490586", 
                                                                                   "rs17012787", "rs4697516", "rs11940750", "rs12507955", "rs17012899")), "plotly")
  expect_warning(qqly(subset(HapMap, CHR %in% 4), snp = "SNP", highlight = significantSNP), "don't exist in your results")
  
  expect_error(qqly(subset(HapMap, CHR %in% 4), highlight = significantSNP), "You're trying to highlight snps, but havent")
})




