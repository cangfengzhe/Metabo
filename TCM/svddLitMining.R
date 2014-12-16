# svdd 预测 ，分??<U+393C><U+3E30>-症状数目??<U+383C><U+3E63>
# 分子数目

# 
library(sqldf)
svdd <- read.csv("result//final//network//predict.csv", stringsAsFactors = F)
molCount <- read.csv("result//final//network//molCount.csv", stringsAsFactors = F)
molSym <- read.csv("result//final//network//molSym.csv", stringsAsFactors = F)

View(molCount)
yuzhi <- 0

score <- function(yuzhi) {
    
    svddYuzhi <- svdd[svdd[, 2] >= yuzhi, ]
    molSymYuzhi <- molSym[molSym10052 - 549[, 2] >= yuzhi, ]
    out <- list()
    # 阈值以上有症状的分子数盐�<U+3E65>
    out$molSymCount <- nrow(molSymYuzhi)
    outlier <- data.frame(id = setdiff(svddYuzhi[, 1], molSymYuzhi[, 1]))
    # outlier中分子的pmid数目
    
    # outlier[, 1] <- as.character(outlier[, 1])
    outlierLitCount <- sqldf("select * from outlier left join molCount on outlier.id=molCount.ID")
    
    outlierCount <- nrow(outlierLitCount)
    outlierYouwenxian <- nrow(na.omit(outlierLitCount))
    # 阈值以上，有文献的outlier数目
    out$scoreYuzhi <- outlierYouwenxian/outlierCount
    out
    
}


score(0)



x <- mapply(score, seq(0, 0.26627, 0.001))

y <- t(x)
View(y)
plot(y)
plot(x$scoreYuzhi, mapply(score, seq(0, 0.26627, 0.001))$molSymCount, type = "l")

plot(y, type = "l", col = "red")

str(x)

svddLit <- sqldf("select * from svdd left join molCount on svdd.ID=molCount.ID")
svddLitCount <- nrow(svddLit)
svddYouwenxianCount <- nrow(na.omit(svddLit))
scoreAll <- svddYouwenxianCount/svddLitCount



 
