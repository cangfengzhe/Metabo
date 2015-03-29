# 修正文件见pubmed.R
load("stroke.rdata")
library(ff)
library(ffbase)

pmid = ""
title = ""
abstract = ""
journal = ""
year = ""
result <- data.frame(pmid, title, abstract, journal, year)
result <- as.ffdf(result)
result0 <- result

preStr <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id="
typeStr <- "&retmode=xml&rettype=abstract"
for (ii in 1:100) {
    tryCatch({
        url <- paste(c(preStr, idList, typeStr), collapse = "")
        result <- pubmedText(url)
        result0 <- ffdfappend(result0, result, adjustvmode = F)
        print(ii)
    }, error = function(e) {
        n <- n + 1
        error0[n] <- ii
        print(ii)
        print(e)
    })
    
}
length(stroke) 
