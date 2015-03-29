mol <- read.csv("./data/5903molecule.csv", stringsAsFactors = F)
library(RSQLite)
conn <- dbConnect(RSQLite(), "./sql.db")
pmidLen <- NA
pmidList <- NA
errorOut <- NA
jj <- 0

result <- sapply(1:nrow(mol), function(ii) {
    tryCatch({
        pmid <- newDownload(mol[ii, 2])
        pmidLen[ii] <<- pmid <- paste(pmid, collapse = ",")
        
        df <- as.data.frame(id = mol[ii, 1], name = mol[ii, 2], 
            count = mol[ii, 3], count2 = length(pmid), detail = pmid)
        print(ii)
    }, error = function(e) {
        jj <<- jj + 1
        print(e)
        errorOut[jj] <<- ii
    })
    
}) 
