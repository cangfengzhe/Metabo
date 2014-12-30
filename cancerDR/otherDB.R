
library(splitstackshape)

# drugbank data ----
drugbank <- read.csv('./data//drugbank.csv', stringsAsFactors = F)
View(drugbank)

# NCBI gene----
geneData <- read.delim('./data/Homo_sapiens.gene_info')
geneInfo <- geneData[,2:5] 
geneInfo <- cSplit(geneInfo, splitCols = 'Synonyms', '|', direction = 'long')
geneInfo <- as.data.frame(geneInfo)
geneInfo <- geneInfo[,c(1,2,4)]

write.csv(geneInfo, file = 'data/geneInfo.csv')


# miRBase 
miRBase <- readLines('data/miRBase.dat')
head(miRBase)
miRText <- paste(miRBase, collapse = ' ')
## Match data from regexpr()
pattern <- 'ID.+?XX.+?AC.+?XX'
m <- gregexpr(pattern, miRText, perl = T)
m

str(miRText)

