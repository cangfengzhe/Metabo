library(psych)
install.packages("psych")
USJudgeRatings
class(USJudgeRatings)
row.names(USJudgeRatings)
X = cor(USJudgeRatings)
ev <- eigen(X)  #特征值 和特征向量
tezhenggen <- ev$value
tezhengvector <- ev$vectors
zaihe <- sqrt(tezhenggen) * tezhengvector
tezhenggen
sort(tezhenggen)
zaihe <- c(0.7985, 0.3968, 0.2392, 0.7336, 0.5826, 0.1436, 0.934, -0.1202, 
    -0.2443, 0.9052, -0.3674, 0.0682, 0.9228, -0.2361, -0.2085, 0.8661, -0.4543, 
    0.0535, 0.7246, 0.4344, -0.4752, 0.8613, -0.0032, 0.4186)
zaihe <- matrix(zaihe, 8, 3, byrow = "T")
rowData <- read.table("ex.txt")
rowData <- as.matrix(rowData)
rowData %*% zaihe
ev <- eigen(var(rowData))  #特征值 和特征向量
tezhenggen <- ev$value
tezhengvector <- ev$vectors
zaihe <- sqrt(tezhenggen[2]) * tezhengvector[, 2]
zaihe
tezhenggen
tezhengvector
PCA <- princomp(rowData, cor = "T")
summary(PCA)
predict(PCA)
rowData %*% zaihe
test <- cumsum(tezhenggen)/sum(tezhenggen)
test
prcomp(rowData, scale = T, )
library(psych)
principal(rowData)
library(RODBC)
db <- odbcConnectExcel2007("E:\\统计\\多元统计分析及R语言建模\\mvcase2.xls")
odbcClose(db)

sqlTables(db)
E8.3 <- sqlFetch(db, "E8#3")
data <- E8.3[, 2:length(colnames(E8.3))]
data <- scale(data)

cor <- cor(data)
cov <- cov(data)
ev <- eigen(cor)
tezhengzhi <- ev$values
tezhengvector <- ev$vectors
rm(list = "zaihe1")
zaihe1 <- matrix(data = NA, 8, 8)
for (ii in 1:8) {
    zaihe1[, ii] <- sqrt(tezhengzhi[ii]) * tezhengvector[, ii]
}
result1 <- cor %*% zaihe1
cor(result1[, 2], result1[, 8])
result1 <- data %*% zaihe1
var(result1[, 4])
result1 <- data %*% tezhengvector
zaihe1 <- as.matrix(zaihe1)
data

pca <- princomp(data, cor = T)

summary(pca)
pca$loading
result <- pca$score
cor(result[, 1], result[, 2])
data[, c(1, 2, 3)]
data
scale(data)
cor(result)
# R语言实战中的函数
result <- principal(data, nfactors = 8, scores = T, rotate = "none")
lie1 <- result$loading[, 1]
lie1 <- as.matrix(lie1)
data[1, ] %*% lie1[, 1]
result$scores

source("http://bioconductor.org/biocLite.R")
biocLite("limma")
round(1.567, 1) 
