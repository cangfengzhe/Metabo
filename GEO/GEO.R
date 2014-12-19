# 利用GEO中的甲基化数据进行差异分析
library(GEOquery)
source("http://bioconductor.org/biocLite.R")
biocLite("GEOquery")
biocLite("wateRmelon")
biocLite("COHCAP")
library(wateRmelon)

geoData <- getGEO(GEO='GSE28647', filename='D:\\work\\GSE28647_family.soft\\GSE28647_family.soft', GSEMatrix = T)
str(geoData)
geoData$contact_city
class(geoData)
geoData@gsms$GSM709793 #%>% View()
GSMList(geoData)[[2]] %>% Table()

test <- GPLList(geoData)[[1]] %>% Table()
test$ID
aa <-GSMList(geoData)

Table(aa[[1]])
exprs(geoData[[1]])

