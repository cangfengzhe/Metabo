library(MASS)  #-------载入包。
library(RODBC)
db <- odbcConnectExcel2007("E:\\统计\\多元统计分析及R语言建模\\mvstats2.xls")
d6.1 <- sqlFetch(db, "d6#1")
data <- d6.1[, -4]
attach(data)
lda <- lda(G ~ x1 + x2, data)  #------ Fisher线性判别法计算(用公式).这里formula是指判别公式；data是指数据文件；prior =是指先验分布。
summary(lda)
lda$predict(lda, data[, 2:3])  #--------预测新样品的类。这里z是指Fisher线性判别法计算结果；newdata是指新样品。



# 距离判别 马氏距离
install.packages("mvstats")
library(mvstats)
g1 <- data[G == "1", 2:3]
g2 <- data[G == "2", 2:3]
mean1 <- colMeans(g1)
mean2 <- colMeans(g2)
mahalanobis(data[6, 2:3], mean1, cov(data[G == "1", 2:3]))
mahalanobis(data[6, 2:3], mean2, cov(data[G == "1", 2:3])) 
