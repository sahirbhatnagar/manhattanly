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


test_that("qqr objects on HapMap data", {
  expect_s3_class(DT_default,"qqr")
  expect_s3_class(DT_snp_gene,"qqr")
  expect_s3_class(DT2_default,"qqr")
  expect_s3_class(DT2_snp_gene,"qqr")
  expect_s3_class(DT_annotations, "qqr")
  
  
  expect_type(DT2_default, "list")
  expect_type(DT2_snp_gene, "list")
  
  
  expect_equal(DT_annotations[["pName"]], "Pval")
  expect_equal(DT_annotations[["snpName"]], "rs")
  expect_equal(DT_annotations[["geneName"]], "taxa")
  expect_equal(DT_annotations[["annotation1Name"]], "zvalue")
  expect_equal(DT_annotations[["annotation2Name"]], "chromosome")
  
  # check for numeric columns  P 
  expect_error(qqr(transform(HapMap, P = as.character(P))), "this column is not numeric")
  
  # columns not found
  expect_error(qqr(HapMap2, p = "P"), "Column P not found")

  # missing column name
  expect_error(qqr(HapMap,  snp = "pt"), "snp argument specified as pt but this column not found")
  expect_error(qqr(HapMap,  gene = "pt"), "gene argument specified as pt but this column not found")
  expect_error(qqr(HapMap,  annotation1 = "pt"), "annotation1 argument specified as pt but this column not found")
  expect_error(qqr(HapMap,  annotation2 = "pt"), "annotation2 argument specified as pt but this column not found")
  
  # check for negative p-values
  expect_error(qqr(transform(HapMap, P = -P)), "Negative p-values found")
  expect_error(qqr(transform(HapMap, P = P+1)), "P-values greater than 1 found.")
  expect_error(qqr(transform(HapMap, P = c(NA, NA, rep(0.5, nrow(HapMap)-2)))), "NA P-values found.")
  
  
})
