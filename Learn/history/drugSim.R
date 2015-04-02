drugFinger <- read.csv("C:\\Users\\Administrator\\Desktop\\similar\\drug_fingerprint.csv", 
    header = F)  #ҩ??ָ??ͼ??
drugTarget <- read.csv("E:\\code\\drugTarget\\drugTarget.csv")  #ҩ??-?е??໥??ϵ
colnames(drugTarget) <- c("drugTargetID", "drugID", "targetID", "action")
colnames(drugTarget)

library(RMySQL)
conMysql <- dbConnect(dbDriver("MySQL"), dbname = "drugTarget", user = "root", 
    password = "12345678")
dbWriteTable(conMysql, "drugTarget", drugTarget)
drugTarget2 <- matrix(data = NA, 17591346, 12)
rm(drugTarget2)
5932 * 5931/2

n = 0

for (ii in 1:5931) {
    for (jj in ii:5931) {
        n = n + 1
        drugTarget2[n, 1:3] <- drugTarget[ii, 2:4]
        drugTarget2[n, 4:6] <- drugTarget[jj, 2:4]
        
    }
}
rm(drugTarget2)
aa[122, 5] <- 4
aa 
