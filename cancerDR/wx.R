# Wang Xia's work read the fasta data----

fileList <- dir("D:/work/WX/", pattern = ".*faa$", full.names = T)
datadf <- matrix(NA, 80000, 7)
pattern <- ">gi"
ii <- 0

sapply(fileList, function(xx) {
    
    fileData <- readLines(xx)
    
    # head(fileData)
    
    proList <- grep(pattern, x = fileData)  #
    
    proLen <- length(proList)
    sapply(1:proLen, function(x) {
        # 
        dataHead <- strsplit(fileData[proList[x]], "\\|")
        ii <<- ii + 1
        datadf[ii, 1:3] <<- unlist(dataHead)[c(2, 4, 5)]
        if (x != proLen) {
            
            datadf[ii, 4] <<- paste(fileData[(proList[x] + 1):(proList[x + 
                1] - 1)], collapse = "")
        } else {
            
            datadf[ii, 4] <<- paste(fileData[(proList[x] + 1):length(fileData)], 
                collapse = "")
        }
        
        print(ii)
    })
})

# regular expression
library(plyr)
pattern <- "[AVLIPFWM][GSYCNQTKRHDE]S[ST]{2}"
resultIndex <- gregexpr(pattern, datadf[, 4])

resultPos <- ldply(1:80000, function(x) {
    paste(resultIndex[[x]], collapse = ", ")
})
resultLen <- ldply(1:80000, function(x) {
    length(resultIndex[[x]])
})

result <- regmatches(datadf[, 4], resultIndex)
result1 <- ldply(1:80000, function(x) {
    paste(result[[x]], collapse = ", ")
})


datadf[, 5] <- resultLen[, 1]
datadf[, 6] <- resultPos[, 1]
datadf[, 7] <- result1[, 1]
write.csv(datadf[1:ii, 1:4], file = "./data/wx_proteinSequence.csv")
dataResult <- datadf[which(datadf[, 6] != "-1"), ]
nrow(dataResult)
write.csv(dataResult, file = "./data/wx_proteinResult.csv") 
