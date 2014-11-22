# 寑�<U+3E39> 少于1万的进行处理
View(meshWan)
save(meshWan, molWan, file = "lessWan.rdata")
meshWan[, 3] <- paste("a", 1:nrow(meshWan), sep = "")
load("lessWan.rdata")
ii = 1



meshError <- NA
n <- 0
for (ii in 1:nrow(meshWan)) {
    tryCatch({
        str <- paste(meshWan[ii, 3], "<-downloadPmid(\"", meshWan[ii, 1], 
            "\")", collapse = "")
        eval(parse(text = str))
        print(ii)
    }, error = function(e) {
        print(e)
        n <<- n + 1
        meshError[n] <- ii
    })
}

result <- matrix(NA, nrow(meshWan) * nrow(molWan), 4)
molWan[, 1] <- as.character(molWan[, 1])
meshWan[, 1] <- as.character(meshWan[, 1])

result <- matrix(NA, nrow(meshWan) * nrow(molWan), 4)
ii = 1
jj = 1
mm <- 0
nn <- 0
flag <- 0
molError <- NA

for (ii in 5423:nrow(molWan)) {
    flag <<- 0
    tryCatch({
        molPmid <- downloadPmid(molWan[ii, 1])
    }, error = function(e) {
        mm <<- mm + 1
        molError[nn] <- ii
        flag <<- 1
    })
    print(paste("ii", ii))
    if (flag == 0) {
        
        for (jj in 1:nrow(meshWan)) {
            
            str <- paste("tmp<-intersect(", meshWan[jj, 3], ",molPmid)")
            eval(parse(text = str))
            if (length(tmp) > 0) {
                nn <- nn + 1
                result[nn, 1] <- molWan[ii, 1]
                result[nn, 2] <- meshWan[jj, 1]
                result[nn, 3] <- length(tmp)
                result[nn, 4] <- paste(tmp, collapse = ",")
                print(jj)
            }
            
        }
        
    }
}



aa <- matrix(1:10, 10, 1)
bb <- matrix(11:20, 10, 1)
cc <- intersect(aa, bb)
length(cc) 
