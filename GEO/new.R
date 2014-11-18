source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite('affy')
library('affy')
biocLite('affylmGUI')
library('affylmGUI')
biocLite('gcrma')

affydata <- ReadAffy(celfile.path = "D:\\deskTop\\CKM\\GSE48620_RAW")
eset.mas5 = mas5(affydata) #无log
exprSet.nologs = exprs(eset.mas5)
exprSet = log(exprSet.nologs, 2)

expMean<-mean(exprSet[,1:3])
View(expMean)3
