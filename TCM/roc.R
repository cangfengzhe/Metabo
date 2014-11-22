# svdd数据与文献挖掘数据对??<U+393C><U+3E34>

data <- read.csv("result\\final\\network\\svddpredict.csv", stringsAsFactor = F)
View(data)
data[, 3] <- as.numeric(data[, 3])
hist(data[, 3])

predict <- read.csv("result\\final\\network\\predict.csv", header = F, stringsAsFactor = F)
View(predict)
class(predict[, 2])
roc <- function(score) {
    litCount <- length(which(data[, 3] > score))
    svddCount <- length(which(predict[, 2] > score))
    outlierCount <- svddCount - litCount
    outlierCount
}
aa <- seq(from = -0.43011, to = 0.266, by = 0.01)

litCount <- function(score) {
    length(which(data[, 3] > score))
    
}
litCount(0.1)
wenxianCount <- mapply(litCount, aa)
outlier <- mapply(roc, aa)
library(ggplot2)
df <- data.frame(yuzhi = aa, wenxianCount = wenxianCount, outlier = outlier)
ggplot(df, aes(yuzhi, outlier)) + geom_line() + geom_line(aes(yuzhi, wenxianCount))


plot(data[, 3], type = "l")
hist(data[, 3]) 



#采用pRCO package 计算ROC曲线

rocData <- read.csv('result//final//network//roc.csv', stringsAsFactors=F, header = F)

roc(response = rocData[,3], predictor = rocData[,2], plot=T)
