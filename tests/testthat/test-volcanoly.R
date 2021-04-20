data("HapMap")
DT_default <- volcanor(HapMap, 
                       p = "P",
                       effect_size = "EFFECTSIZE")

#include snp and gene information
DT_snp_gene <- volcanor(HapMap, 
                        p = "P",
                        effect_size = "EFFECTSIZE",
                        snp = "SNP",
                        gene = "GENE")
# head(DT_snp_gene[["data"]])


# change colnames
HapMap2 <- HapMap
colnames(HapMap2) <- c("chromosome","baseposition","Pval","rs","zvalue", "logFC","taxa","dist")


DT2_default <- volcanor(HapMap2, 
                        p = "Pval",
                        effect_size = "logFC")
DT2_snp_gene <- volcanor(HapMap2, 
                         p = "Pval",
                         effect_size = "logFC",
                         snp = "rs",
                         gene = "taxa")

# add annotations
DT_annotations <- volcanor(HapMap2, 
                           p = "Pval",
                           effect_size = "logFC",
                           snp = "rs",
                           gene = "taxa",
                           annotation1 = "zvalue",
                           annotation2 = "chromosome")

test_that("volcanoly on HapMap", {
  skip_on_cran()
  expect_s3_class(volcanoly(DT_default), "plotly")
  expect_s3_class(volcanoly(HapMap), "plotly")
  expect_s3_class(volcanoly(HapMap, genomewideline = FALSE, effect_size_line = FALSE), "plotly")
  
  
  expect_s3_class(volcanoly(HapMap, snp = "SNP"), "plotly")
  expect_s3_class(volcanoly(HapMap, snp = "SNP", genomewideline = FALSE), "plotly")
  expect_s3_class(volcanoly(HapMap, snp = "SNP", effect_size_line = FALSE), "plotly")
  
  expect_s3_class(volcanoly(DT2_snp_gene, highlight = sample(DT2_snp_gene$data$rs,10), 
                            highlight_color = "green", col = "red", point_size = 8), "plotly")

  expect_s3_class(volcanoly(HapMap, snp = "SNP", highlight = significantSNP), "plotly")
  
  expect_s3_class(volcanoly(HapMap, snp = "SNP", highlight = significantSNP, genomewideline = FALSE), "plotly")
  
  expect_s3_class(volcanoly(HapMap, snp = "SNP", highlight = significantSNP, 
                            genomewideline = FALSE, effect_size_line = FALSE), "plotly")
  
  expect_s3_class(volcanoly(subset(HapMap, CHR %in% 4), snp = "SNP", highlight = c("rs7689106", "rs4242017", "rs1980271", "rs13117731", "rs1490586", 
                                                                                     "rs17012787", "rs4697516", "rs11940750", "rs12507955", "rs17012899")), "plotly")
  expect_warning(volcanoly(subset(HapMap, CHR %in% 4), snp = "SNP", highlight = significantSNP), "don't exist in your results")
  
  expect_error(volcanoly(subset(HapMap, CHR %in% 4), highlight = significantSNP), "You're trying to highlight snps, but havent")
})
