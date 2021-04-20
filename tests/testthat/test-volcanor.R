test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

devtools::load_all("~/git_repositories/manhattanly/")
help(volcanor)
help(volcanoly)

volcanorObj <- volcanor(HapMap, 
                        p = "P",
                        effect_size = "EFFECTSIZE",
                        snp = "SNP",
                        gene = "GENE")
class(volcanorObj)
head(volcanorObj$data)
volcanoly(volcanorObj, highlight = NULL, highlight_color = "green", col = "red", point_size = 8)

volcanoly(volcanorObj, highlight = sample(volcanorObj$data$SNP,10), highlight_color = "green", col = "red", point_size = 8)

# change colnames
HapMap2 <- HapMap
colnames(HapMap2) <- c("chromosome","baseposition","Pval","rs","zvalue", "logFC","taxa","dist")

volcanorObj2 <- volcanor(HapMap2, 
                        p = "Pval",
                        effect_size = "logFC",
                        snp = "rs",
                        gene = "taxa")

head(volcanorObj2$data)
volcanoly(volcanorObj2)

help(volcanoly)
