data("HapMap")
DT_default <- manhattanr(HapMap, chr = "CHR", bp = "BP", p = "P", logp = FALSE)

#include snp and gene information
DT_snp_gene <- manhattanr(HapMap, snp = "SNP", gene = "GENE")
# head(DT_snp_gene[["data"]])


# change colnames
HapMap2 <- HapMap
colnames(HapMap2) <- c("chromosome","baseposition","pvalue","rs","zvalue", "beta","genename","dist")

DT2_default <- manhattanr(HapMap2, chr = "chromosome", bp = "baseposition", p = "pvalue")
DT2_snp_gene <- manhattanr(HapMap2, chr = "chromosome", bp = "baseposition", p = "pvalue", snp = "rs", gene = "genename")

# single chromosome
HapMap4 <- subset(HapMap, CHR %in% 4)
DT_single <- manhattanr(HapMap4)


# add annotations
DT_annotations <- manhattanr(HapMap, chr = "CHR", bp = "BP", p = "P", snp = "SNP", gene = "GENE",
                             annotation1 = "ZSCORE", annotation2 = "EFFECTSIZE")


test_that("manhattanly on HapMap", {
  skip_on_cran()
  expect_s3_class(manhattanly(DT_default), "plotly")
  
  expect_s3_class(manhattanly(HapMap, suggestiveline = FALSE, genomewideline = FALSE), "plotly")
  expect_s3_class(manhattanly(HapMap, suggestiveline = FALSE, genomewideline = -log10(1e-4)), "plotly")
  expect_s3_class(manhattanly(HapMap, suggestiveline = -log10(1e-4), genomewideline = FALSE), "plotly")
  
  expect_s3_class(manhattanly(HapMap), "plotly")
  expect_s3_class(manhattanly(DT_single), "plotly")
  
  expect_s3_class(manhattanly(HapMap, snp = "SNP", highlight = significantSNP), "plotly")
  expect_s3_class(manhattanly(subset(HapMap, CHR %in% 4), snp = "SNP", highlight = c("rs7689106", "rs4242017", "rs1980271", "rs13117731", "rs1490586", 
                                                                                     "rs17012787", "rs4697516", "rs11940750", "rs12507955", "rs17012899")), "plotly")
  expect_warning(manhattanly(subset(HapMap, CHR %in% 4), snp = "SNP", highlight = significantSNP), "don't exist in your results")
  
  expect_error(manhattanly(subset(HapMap, CHR %in% 4), highlight = significantSNP), "You're trying to highlight snps, but havent")
})
