disease <- read.csv("meshDiseaseAll.csv", stringsAsFactors = F, header = F)
# 分子 先与Signs and Symptoms匹配，有文献
# 的再进行下一步的匹配
errorRes <- matrix(NA, 10000, 2)
warnRes <- matrix(NA, 10000, 2)
result <- matrix(NA, nrow(disease), 4)
n <- 0
m <- 0
kk <- 0

for (ii in 1:nrow(disease)) {
    
    molName <- "stroke"
    meshName <- disease[ii, 1]
    
    
    tryCatch({
        
        prestr = paste(c("(\"", molName, "\" [MeSH Terms] OR \"", molName, 
            "\" [Title/Abstract]) AND ", "(\"", meshName, "\" [MeSH Terms] OR \"", 
            meshName, "\"[Title/Abstract])"), collapse = "")
        pmid <- downloadPmid(prestr)
        if (is.matrix(pmid)) {
            kk <- kk + 1
            pmid0 <- paste(pmid, collapse = ",")
            result[kk, 1] <- molName
            result[kk, 2] <- meshName
            result[kk, 4] <- pmid0
            result[kk, 3] <- nrow(pmid)
            
            
        }
    }, error = function(e) {
        n <<- n + 1
        errorRes[n, 1] <<- ii
        
        print(e)
    }, warning = function(w) {
        m <<- m + 1
        warnRes[m, 1] <<- ii
        
        print(w)
    })
    
    
    print(paste("ii", ii))
} 
