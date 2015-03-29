# To process the lncRNA data associated to the cancer drug
# resistance from literature
library(dplyr)
library(splitstackshape)
library(stringr)
library(splitstackshape)
# import data ----
rawLncRNA <- read.csv("data/lncRNA.csv", stringsAsFactors = F)
# View(rawLncRNA) colnames(rawLncRNA) process data---- split
# the strings from cell table separated by '|'
splitLncRNA <- rawLncRNA %>% cSplit("cell_lines", "|", "long") %>% 
    cSplit("disease", "|", "long") %>% cSplit("drug", "|", "long") %>% 
    cSplit("lncRNA", "|", "long")

# from data.table to data.frame
splitLncRNA <- as.data.frame(splitLncRNA)

# trim the string
procLncRNA <- sapply(1:ncol(splitLncRNA), function(x) {
    splitLncRNA$x <- as.character(splitLncRNA[, x])
    str_trim(splitLncRNA[, x])
})
colnames(procLncRNA) <- colnames(rawLncRNA)
# export data to local file , then to process in the excel
write.csv(procLncRNA, "data/procLncRNA.csv") 
