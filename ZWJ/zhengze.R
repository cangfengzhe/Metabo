`?`(grep)
weiWenxian <- read.csv("wei-wenxian.csv", stringsAsFactors = F)
class(weiWenxian[, 3])
aa <- NA
for (ii in 1:nrow(weiWenxian1)) {
    
    tmp <- grepl(weiWenxian1[ii, 3], weiWenxian1[ii, 7], ignore.case = T)
    
    if (tmp == T) {
        
        aa[ii] <- tmp
    }
    
}


View(weiWenxian)

aa <- as.integer(aa)
bb <- which(aa == 1)

weiWenxian2 <- weiWenxian1[bb, ]
write.csv(weiWenxian2, file = "wei-wenxian0827.csv")
e 
