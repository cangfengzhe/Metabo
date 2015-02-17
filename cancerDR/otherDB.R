
library(splitstackshape)

# import the data from drugbank data ----
drugbank <- read.csv("./data//drugbank.csv", stringsAsFactors = F)
View(drugbank)

# import the data from NCBI gene----
geneData <- read.delim("./data/Homo_sapiens.gene_info")
geneInfo <- geneData[, 2:5]
geneInfo <- geneInfo  %>% 
  cSplit(splitCols = "Synonyms", "|", direction = "long") %>%
  as.data.frame()
geneInfo <- geneInfo[, c(1, 2, 4)]

write.csv(geneInfo, file = "data/geneInfo.csv")


# miRBase----
miRBase <- readLines("data/miRBase.dat")
head(miRBase)

# # Match data from regexpr()
miRText <- paste(miRBase, collapse = '\n')
ireg <- gregexpr('\\bID[\\s\\S]*?DE\\b', miRText, perl =T)
miR <- regmatches(miRText, m = ireg)
miRVec <- unlist(miR)
# head(miRVec)

miRList <- strsplit(miRVec, '\\s{1,}')

miRdf <- do.call('rbind',miRList) #
miRdfProc <- miRdf[,c(2,5,6,10)]
colnames(miRdfProc) <- c('index', 'species', 'bp', 'ID')

write.csv(miRdfProc, file = 'data/miRbaseDB_proc.csv')


aa <- function(){
  bb <- function(){
    x <<- 3
    b <- 5
    envbb <<- environment()
    }
  bb()
  cx <- x;
  envaa <<- environment()
}

aa()
cx
ls(envir=envaa)



