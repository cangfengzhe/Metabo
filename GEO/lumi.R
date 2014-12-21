# lumi package is used to process the Illumina Infinium 27k and 450k methylation microarray data

# 采用lumi自带的数据
library(lumi)
#导入GEO数据 
# exprs data

#GSE28647_signal_intensities.txt
fileName <- 'D:/work/GSE28647/GSE28647_signal_intensities.txt'
rawData <- read.delim(fileName, stringsAsFactors=F)
names(rawData)

rawDataTbl <- tbl_df(rawData)
rawDataMeth <- select(rawDataTbl,ends_with('.methylated.signal')) #需加'.'与unmethylated.signal区别
rawDataUnmeth <- select(rawDataTbl,ends_with('unmethylated.signal'))
betaValue <- rawDataMeth/( rawDataMeth + rawDataUnmeth+100)
sampleName <- do.call('cbind',strsplit(colnames(betaValue),'[.]'))
colnames(betaValue) <- sampleName[1,]
row.names(betaValue) <- rawData[,1]
# detective pvalue
pvalue <- select(rawDataTbl,ends_with('Pval'))
colnames(pvalue) <- sampleName[1,]

# 导入lumi
# 新建LumiBatch 实例
# 因为没有数据的标准差 se.expr,所以我们建立se.expr为NA的矩阵
betaValue <- as.matrix(betaValue)
pvalue <- as.matrix(pvalue)
se <- matrix(0,nrow(betaValue), ncol(betaValue))
lumiData <- new('LumiBatch', exprs=betaValue, se.exprs=se, detection<-pvalue)

plot(lumiData, what='density')
plotCDF(lumi.N.Q)

lumi.B <-lumiB(lumiData)

lumi.N.Q <- lumiExpresso(lumiData, QC.evaluation=TRUE, variance.stabilize=F)

plotCDF(lumi.N.Q)
plot(lumi.N.Q, what='density')
limiQ(lumi.N.Q)
summary(lumi.N.Q,'QC')
plot(lumi.N.Q, what='density')
