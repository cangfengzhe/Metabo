
# 计算一个关键词出现次数

disease <- read.csv("meshDisease.csv", header = F, stringsAsFactors = F)
View(disease)
errRes <- NA
kk <- 0
for (ii in 1:nrow(disease)) {
    tryCatch({
        keyword <- disease[ii, 1]
        keywordQuery <- paste(c("\"", keyword, "\" [Majr:NoExp]"), collapse = "")
        disease[ii, 3] <- pmidCount(keywordQuery)
        print(paste(c(ii, "/9890"), collapse = ""))
    }, error = function(e) {
        print(e)
        kk <<- kk + 1
        errRes[kk] <<- ii
    })
    
}
# 取疾病数目大�<U+383C><U+3E65>0�<U+383C><U+3E34>
disease <- read.csv("diseasePmidCount.csv", stringsAsFactors = F)
View(disease)
class(disease[, 3])
disease[, 3] <- as.numeric(disease[, 3])
dis <- disease[disease[, 3] > 0, ]
nrow(dis)
write.csv(dis, file = "diseasePmidCount.csv")
# disease[,3]<-as.numeric(disease[,3])
save.image()
nrow(dis)
disease[3, 1]
disease2 <- matrix(NA, 10000 * 10000/2, 3)


# 获得疾病两两组合名称
kk <- 0

for (ii in 1:nrow(dis)) {
    for (jj in (ii + 1):nrow(dis)) {
        kk <- kk + 1
        disease2[kk, 1] = dis[ii, 1]
        disease2[kk, 2] = dis[jj, 1]
        
    }
    print(ii)
}

kk 
