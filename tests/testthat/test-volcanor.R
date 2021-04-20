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

volcanorObj2 <- volcanor(HapMap2, 
                         p = "Pval",
                         effect_size = "logFC",
                         snp = "rs",
                         gene = "taxa",
                         annotation1 = "zvalue",
                         annotation2 = "chromosome")

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


test_that("volcanor objects on HapMap data", {
  expect_s3_class(DT_default,"volcanor")
  expect_s3_class(DT_snp_gene,"volcanor")
  expect_s3_class(DT2_default,"volcanor")
  expect_s3_class(DT2_snp_gene,"volcanor")
  expect_s3_class(DT_annotations, "volcanor")
  
  
  expect_type(DT2_default, "list")
  expect_type(DT2_snp_gene, "list")
  
  
  expect_equal(DT_annotations[["pName"]], "Pval")
  expect_equal(DT_annotations[["effectName"]], "logFC")
  expect_equal(DT_annotations[["xlabel"]], "logFC")
  expect_equal(DT_annotations[["snpName"]], "rs")
  expect_equal(DT_annotations[["geneName"]], "taxa")
  expect_equal(DT_annotations[["annotation1Name"]], "zvalue")
  expect_equal(DT_annotations[["annotation2Name"]], "chromosome")
  
  # check for numeric columns  P and effect size
  expect_error(volcanor(transform(HapMap, EFFECTSIZE = as.character(EFFECTSIZE))), "this column is not numeric")
  expect_error(volcanor(transform(HapMap, P = as.character(P))), "this column is not numeric")
  
  # columns not found
  expect_error(volcanor(HapMap2, p = "P"), "Column P not found")
  expect_error(volcanor(HapMap2, effect_size = "effect_estimate", p = "Pval"), "Column effect_estimate not found")
  
  # missing column name
  expect_error(volcanor(HapMap,  snp = "pt"), "snp argument specified as pt but this column not found")
  expect_error(volcanor(HapMap,  gene = "pt"), "gene argument specified as pt but this column not found")
  expect_error(volcanor(HapMap,  annotation1 = "pt"), "annotation1 argument specified as pt but this column not found")
  expect_error(volcanor(HapMap,  annotation2 = "pt"), "annotation2 argument specified as pt but this column not found")
  
  # check for negative p-values
  expect_error(volcanor(transform(HapMap, P = -P)), "Negative p-values found")
  expect_error(volcanor(transform(HapMap, P = P+1)), "P-values greater than 1 found.")
  expect_error(volcanor(transform(HapMap, P = c(NA, NA, rep(0.5, nrow(HapMap)-2)))), "NA P-values found.")
  
  
})

# devtools::load_all("~/git_repositories/manhattanly/")
# help(volcanor)
# help(volcanoly)
# 
# volcanorObj <- volcanor(HapMap, 
#                         p = "P",
#                         effect_size = "EFFECTSIZE",
#                         snp = "SNP",
#                         gene = "GENE")
# class(volcanorObj)
# head(volcanorObj$data)
# volcanoly(volcanorObj, highlight = NULL, highlight_color = "green", col = "red", point_size = 8)
# 
# volcanoly(volcanorObj, highlight = sample(volcanorObj$data$SNP,10), highlight_color = "green", col = "red", point_size = 8)
# 
# 
# 
# head(volcanorObj2$data)
# volcanorObj2$annotation2Name
# volcanoly(volcanorObj2, effect_size_line_type = 2)
# 
# help(volcanoly)
