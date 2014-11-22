keyword <- read.csv("keyword.csv", stringsAsFactors = F, encoding = "UTF-8")
target <- read.csv("target.csv", stringsAsFactors = F, header = F)
View(keyword)
result <- matrix(NA, 23 * 17, 1)
ii = 1
jj = 1
nn = 0
for (ii in 1:23) {
    for (jj in 1:17) {
        nn <- nn + 1
        result[nn, 1] <- paste("\"", keyword[ii, 1], "\"[Title/Abstract] AND \"", 
            keyword[jj, 2], "\"[Title/Abstract]", collapse = "")
    }
}

result0 <- result
errorRes <- matrix(NA, 10000, 2)
warnRes <- matrix(NA, 10000, 2)
result <- matrix(NA, nrow(target) * nrow(result0), 4)
n <- 0
m <- 0
kk <- 0

for (ii in 3:nrow(target)) {
    for (jj in 1:nrow(result0)) {
        molName <- target[ii, 1]
        meshName <- result0[jj, 1]
        print(paste("jj", jj))
        
        tryCatch({
            
            prestr = paste(c("\"", molName, "\"[Title/Abstract]", " AND ", 
                meshName), collapse = "")
            pmid <- downloadPmid(prestr)
            if (is.matrix(pmid)) {
                kk <- kk + 1
                pmid0 <- paste(pmid, collapse = ",")
                result[kk, 1] <- molName
                result[kk, 2] <- meshName
                result[kk, 4] <- pmid0
                result[kk, 3] <- nrow(pmid)
                
                print(paste("kk", kk))
            }
        }, error = function(e) {
            n <<- n + 1
            errorRes[n, 1] <<- ii
            errorRes[n, 2] <<- jj
            print(e)
        }, warning = function(w) {
            m <<- m + 1
            warnRes[m, 1] <<- ii
            warnRes[m, 2] <<- jj
            print(w)
        })
        
    }
    print(paste("ii", ii))
}

keyword <- read.csv("keyword.csv", stringsAsFactors = F, encoding = "UTF-8")
target <- read.csv("target.csv", stringsAsFactors = F, header = F)
View(keyword)
result <- matrix(NA, 23 * 17, 1)
ii = 1
jj = 1
nn = 0
for (ii in 1:23) {
    for (jj in 1:17) {
        nn <- nn + 1
        result[nn, 1] <- paste("\"", keyword[ii, 1], "\"[Title/Abstract] AND \"", 
            keyword[jj, 2], "\"[Title/Abstract]", collapse = "")
    }
}



# 靶点与胃匹配
View(target)
View(keyword)
xin <- keyword[, 1]
xin <- as.data.frame(xin)
xin[, 1] <- as.character(xin[, 1])

errorRes <- matrix(NA, 10000, 2)
warnRes <- matrix(NA, 10000, 2)
result <- matrix(NA, nrow(target) * nrow(xin), 4)
n <- 0
m <- 0
kk <- 0

for (ii in 1:nrow(target)) {
    for (jj in 1:nrow(xin)) {
        molName <- target[ii, 1]
        meshName <- xin[jj, 1]
        print(paste("jj", jj))
        
        tryCatch({
            
            prestr = paste(c("\"", molName, "\"[Title/Abstract]", " AND \"", 
                meshName, "\"[Title/Abstract]"), collapse = "")
            pmid <- downloadPmid(prestr)
            if (is.matrix(pmid)) {
                kk <- kk + 1
                pmid0 <- paste(pmid, collapse = ",")
                result[kk, 1] <- molName
                result[kk, 2] <- meshName
                result[kk, 4] <- pmid0
                result[kk, 3] <- nrow(pmid)
                
                print(paste("kk", kk))
            }
        }, error = function(e) {
            n <<- n + 1
            errorRes[n, 1] <<- ii
            errorRes[n, 2] <<- jj
            print(e)
        }, warning = function(w) {
            m <<- m + 1
            warnRes[m, 1] <<- ii
            warnRes[m, 2] <<- jj
            print(w)
        })
        
    }
    print(paste("ii", ii))
} 
