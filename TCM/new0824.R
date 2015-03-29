# 采用 [majr:Noexp]
# mesh<-read.csv('mesh.csv',stringsAsFactors=F,header=F)
# mol<-read.csv('molWithPid.csv',stringsAsFactors=F)
molSym <- read.csv("molSymptom.csv", stringsAsFactors = F)
View(molSym)
errorRes <- matrix(NA, 10000, 2)
warnRes <- matrix(NA, 10000, 2)
result <- matrix(NA, nrow(molSym), 4)
n <- 0
m <- 0
kk <- 0

for (ii in 1:nrow(molSym)) {
    
    molName <- molSym[ii, 2]
    meshName <- molSym[ii, 3]
    
    
    tryCatch({
        
        prestr = paste(c("\"", molName, "\"[All Fields] AND ", 
            "\"", meshName, "\"[MeSH Major Topic:noexp]"), collapse = "")
        pmid <- downloadPmid(prestr)
        if (is.matrix(pmid)) {
            
            pmid0 <- paste(pmid, collapse = ",")
            result[ii, 1] <- molName
            result[ii, 2] <- meshName
            result[ii, 4] <- pmid0
            result[ii, 3] <- nrow(pmid)
            
            
        }
    }, error = function(e) {
        n <<- n + 1
        errorRes[n, 1] <<- ii
        # errorRes[n,2]<<-jj;
        print(e)
    }, warning = function(w) {
        m <<- m + 1
        warnRes[m, 1] <<- ii
        # warnRes[m,2]<<-jj;
        print(w)
    })
    
    
    print(paste("ii", ii))
}




aa <- read.csv("./result/final/network/correlation.csv")
View(aa) 
