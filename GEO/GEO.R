# 利用GEO中的甲基化数据进行差异分析
library(GEOquery)
source("http://bioconductor.org/biocLite.R")
biocLite("GEOquery")
biocLite("wateRmelon")
biocLite("COHCAP")
biocLite("IlluminaHumanMethylation450kanno.ilmn12.hg19")
library(wateRmelon)
library(dplyr)
geoData <- getGEO(GEO = "GSE28647", filename = "D:\\work\\GSE28647\\GSE28647_family.soft", 
    GSEMatrix = T)


biocLite()
## 获得 单个GSM数据 Table(GSMList(geoData)[[1]]) %>% View()


## 得到甲基化矩阵 根据GPL平台信息得到探针信息
probesets <- Table(GPLList(geoData)[[1]])$ID

data.matrix <- do.call("cbind", lapply(GSMList(geoData), function(x) {
    tab <- Table(x)
    mymatch <- match(probesets, tab$ID_REF)
    return(tab$VALUE[mymatch])
}))

# 数据转换为numeric
data.matrix <- apply(data.matrix, 2, function(x) {
    as.numeric(as.character(x))
})

# cpg 信息
gpl <- GPLList(geoData)
geneInfo <- Table(gpl[[1]])[, c("ID", "Gene_ID")] %>% View()
summary(geneInfo)
summary(data.matrix) %>% View() 
