XMGene <- read.csv("XM_geneSym.csv", stringsAsFactors = F)
View(XMGene)
guangheGeneCount <- ABPmid
co2guding <- ABPmid
guangheGeneCount0 <- as.numeric()
write.csv(guangheGeneCount, file = "guanghe.csv")
write.csv(co2guding, file = "co2guding.csv")
A <- "\"carbon fixation\""
ACount <- pmidCount(A)
B <- XMGene
ii = 1
ABPmid <- matrix(NA, nrow(B), 5)
AorBCount <- 2.4e+07
kk = 0
errorRes = NA
for (ii in 1:nrow(B)) {
    tryCatch({
        str <- paste(c("\"", B[ii, 2], "\""), collapse = "")
        BCount <- as.numeric(guangheGeneCount[ii, 3])
        ABStr <- paste(c(A, " AND ", str), collapse = "")
        ABCount <- downloadPmid(ABStr)
        # AorBStr<-ABStr<-paste(c(A,' OR ',str),collapse='')
        # AorBCount<-pmidCount(AorBStr);
        mat <- matrix(c(ABCount, ACount - ABCount, BCount - ABCount, 
            AorBCount - ACount - BCount + ABCount), 2)
        pvalue <- chisq.test(mat, correct = T)$p.value
        ABPmid[ii, 1] = B[ii, 1]
        ABPmid[ii, 2] = B[ii, 2]
        ABPmid[ii, 3] = BCount
        ABPmid[ii, 4] = ABCount
        ABPmid[ii, 5] = pvalue
        print(ii)
    }, error = function(e) {
        kk <<- kk + 1
        errorRes[kk] <<- ii
        print(e)
    })
    
} 
