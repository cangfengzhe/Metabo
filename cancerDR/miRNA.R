# To process the miRNA data associated to the cancer drug
# resistance from literature

library(splitstackshape)
library(stringr)
library(splitstackshape)
# import data ----
rawMiRNA <- read.csv("data/miRNA.csv", stringsAsFactors = F)

# process data---- split the strings from cell table separated
# by '|'
splitMiRNA <- rawMiRNA %>% cSplit("cellLines", "|", "long") %>% 
    cSplit("disease", "|", "long") %>% cSplit("drug", "|", "long") %>% 
    cSplit("miRNA", "|", "long")

# from data.table to data.frame
splitMiRNA <- as.data.frame(splitMiRNA)

# trim the string
procMiRNA <- sapply(1:ncol(splitMiRNA), function(x) {
    splitMs$x <- as.character(splitMiRNA[, x])
    str_trim(splitMiRNA[, x])
})
colnames(procMiRNA) <- colnames(rawMiRNA)
# export data to local file , then to process in the excel
write.csv(procMiRNA, "data/procMiRNA.csv") 
