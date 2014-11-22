mol <- read.csv("D:/deskTop/XX/xxMol.csv")
mesh <- read.csv("D:/deskTop/XX/symptomsMesh.csv", header = F)
View(mesh)
library(XML)
mesh[, 1] <- as.character(mesh[, 1])
result <- matrix(NA, nrow(mol), 3)
mol[, 3] <- as.character(mol[, 3])

# 899 _qt molecule
n = 0
m = 0
errorRes <- NA
warningRes <- NA

for (ii in 910:nrow(mol)) {
    # nrow(mol)
    
    tryCatch({
        molName <- mol[ii, 3]
        
        result[ii, 1] <- molName
        result[ii, 2] <- downloadPmid(molName)
        result[ii, 3] <- mol[ii, 2]
        print(ii)
    }, error = function(e) {
        print(e)
        n <<- n + 1
        errorRes[n] <<- ii
    })
}

# errorRes 进一歐�<U+3E35>
n <<- 0
errorRes0 <- NA
for (ii in errorRes) {
    # nrow(mol)
    
    tryCatch({
        molName <- mol[ii, 3]
        
        result[ii, 1] <- molName
        result[ii, 2] <- downloadPmid(molName)
        result[ii, 3] <- mol[ii, 2]
        print(ii)
    }, error = function(e) {
        print(e)
        n <<- n + 1
        errorRes0[n] <<- ii
    })
}
# 提取文献数目�<U+393C><U+3E65>0的表

result_1 <- result[result[, 2] != 0, ]
nrow(result_1)
View(result_1)
downloadPmid("acetoxyatractylone")
save(result_1, file = "xxMOlPmidCount.rdata")
load("xxMOlPmidCount.rdata")
View(result_1)

# 提取 ID
mesh[, 1] <- as.character(mesh[, 1])
errorRes <- matrix(NA, 10000, 2)
warnRes <- matrix(NA, 10000, 2)
result <- matrix(NA, nrow(result_1) * nrow(mesh), 4)
n <- 0
m <- 0
kk <- 0

for (ii in 52:nrow(result_1)) {
    for (jj in 1:nrow(mesh)) {
        molName <- result_1[ii, 1]
        meshName <- mesh[jj, 1]
        print(jj)
        tryCatch({
            
            prestr = paste(c("(", molName, " [MeSH Terms] OR ", molName, 
                "[Title/Abstract])", "(", meshName, " [MeSH Terms] OR ", 
                meshName, "[Title/Abstract])"), collapse = "")
            pmid <- downloadPmid(prestr)
            if (is.data.frame(pmid)) {
                kk <- kk + 1
                pmid0 <- paste(pmid, collapse = ",")
                result[kk, 1] <- molName
                result[kk, 2] <- meshName
                result[kk, 3] <- pmid0
                result[kk, 4] <- nrow(pmid)
                # print(jj)
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
}
system.time(aa <- downloadPmid(mesh[1, 1]))


result <- matrix(NA, nrow(mesh), 2)
errorRes <- NA
n <- 0
for (ii in 1:nrow(mesh)) {
    # nrow(mol)
    
    tryCatch({
        meshName <- mesh[ii, 1]
        
        result[ii, 1] <- meshName
        result[ii, 2] <- downloadPmidCount(meshName)
        # result[ii,3]<-mol[ii,2]
        print(ii)
    }, error = function(e) {
        print(e)
        n <<- n + 1
        errorRes[n] <<- ii
    })
}


meshPmidCount <- result

mesh10000 <- meshPmidCount[meshPmidCount[, 2] < 10000, ]
nrow(mesh10000)
View(mesh10000)
result[1, ]
result <- as.data.frame(result)
result[1, ]
result[, 2] <- as.numeric(result[, 2])
class(result)
class(meshPmidCount)
meshPmidCount[1, 2]
aa <- meshPmidCount
aa[, 2] <- as.numeric(aa[, 2])
aa <- as.data.frame(aa)
aa[, 2] <- as.numeric(meshPmidCount[, 2])
class(aa[, 2])
View(aa)
meshWan <- aa[aa[, 2] < 10000, ]
View(meshWan) 
