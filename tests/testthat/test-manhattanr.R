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

test_that("multiplication works", {
  expect_s3_class(DT_default,"manhattanr")
  expect_s3_class(DT_snp_gene,"manhattanr")
  expect_s3_class(DT2_default,"manhattanr")
  expect_s3_class(DT2_snp_gene,"manhattanr")
  expect_s3_class(DT_single, "manhattanr")
  expect_s3_class(DT_annotations, "manhattanr")
  
  
  expect_type(DT2_default, "list")
  expect_type(DT2_snp_gene, "list")
  
  expect_equal(DT_annotations[["annotation2Name"]], "EFFECTSIZE")
  
  # check for numeric columns CHR, BP, P
  expect_error(manhattanr(transform(HapMap, CHR = as.character(CHR))), "CHR column should be numeric")
  expect_error(manhattanr(transform(HapMap, BP = as.character(BP))), "BP column should be numeric")
  expect_error(manhattanr(transform(HapMap, P = as.character(P))), "P column should be numeric")
  
  # columns not found
  expect_error(manhattanr(HapMap2, chr = "CHR"), "Column CHR not found")
  expect_error(manhattanr(HapMap2, chr = "chromosome", bp = "CHR"), "Column CHR not found")
  expect_error(manhattanr(HapMap2, chr = "chromosome", bp ="baseposition", p = "CHR"), "Column CHR not found")
  
  # missing column name
  expect_error(manhattanr(HapMap,  snp = "pt"), "snp argument specified as pt but this column not found")
  expect_error(manhattanr(HapMap,  gene = "pt"), "gene argument specified as pt but this column not found")
  expect_error(manhattanr(HapMap,  annotation1 = "pt"), "annotation1 argument specified as pt but this column not found")
  expect_error(manhattanr(HapMap,  annotation2 = "pt"), "annotation2 argument specified as pt but this column not found")

})


# manhattanly(subset(HapMap, CHR %in% 4:7), snp = "SNP", gene = "GENE", highlight = significantSNP)
# 
# devtools::load_all()
# tt <- manhattanr(subset(HapMap, CHR %in% 4),snp = "SNP", gene = "GENE")
# manhattanly(tt, snp = "SNP", gene = "GENE", highlight = sample(subset(HapMap, CHR %in% 5)$SNP, 5))
# manhattanly(tt, snp = "SNP", gene = "GENE", highlight = sample(subset(tt$data, CHR %in% 4)$SNP, 5))
# rlang::last_error()
# 
# 
# manhattanly(subset(HapMap, CHR %in% 4), snp = "SNP", gene = "GENE", highlight = significantSNP)
# manhattanly(subset(HapMap, CHR %in% 4), snp = "SNP", gene = "GENE", highlight = sample(subset(tt$data, CHR %in% 4)$SNP, 5))
# 
# tt$data[which(tt$data$SNP %in% significantSNP),]
# significantSNP
# sample(subset(tt$data, CHR %in% 4)$SNP, 5)
