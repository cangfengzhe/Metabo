library(RODBC)
db <- odbcConnectExcel2007("E:\\统计\\多元统计分析及R语言建模\\mvstats2.xls")
d7.2 <- sqlFetch(db, "d7#2")
plot(d7.2)
data <- d7.2[, 2:ncol(d7.2)]
odbcClose(db)
data
# 计算距离
dist <- dist(data, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
plot(dist)
clust <- hclust(dist, method = "complete", members = NULL)  #------系统聚类计算。
plot(clust)
plclust(clust, hang = -1)
rect.hclust(clust, k = 5, border = 6)  #系统聚类图上表示分类。
# 这里d是系统聚类输出结果；k是指定分类数或h分类距离；border是分类方框颜色；which是用于指定要显示成员的类。


# K-mean 聚类
kmean <- kmeans(data, 3, iter.max = 100, nstart = 10)
kmean$centers  #提取最终聚类中心。这里object是指k-mean聚类计算结果。
kmean$cluster  #-------提取最终分类结果。这里object是指k-mean聚类计算结果。
kmean$size  #----------提取每类成员数量。这里object是指k-mean聚类计算结果。
kmean$withinss  #----------提取每类内离差平方和。这里object是指k-mean聚类计算结果。
plot(kmean$cluster) 
