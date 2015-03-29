downloadPmidCount <- function(str) {
    # 检查中药分子的个数 ?\u023b?\u0221???????\u05f5???Ŀ prestr=paste(c(''',str,''
    # [MeSH Terms] OR '',str,'' [Title/Abstract]'),collapse='')
    
    prestr = paste(c(str, " [MeSH Terms] OR ", str, "[Title/Abstract]"), 
        collapse = "")
    # prestr<-str
    preurl <- paste(c("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=", 
        prestr), collapse = "")
    prexmlfile <- htmlParse(preurl, encoding = "UTF-8", asTree = TRUE)  #??\u0221html?ļ?
    countNode <- getNodeSet(prexmlfile, "//count")
    pmidCount <- sapply(countNode, xmlValue)
    pmidCount <- as.data.frame(pmidCount, stringsAsFactors = FALSE)
    retMax <- as.numeric(pmidCount[1, 1])
    
    return(retMax)
}

# url<-paste(c('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=',str,'&RetMax=',retMax,'&RetStart=',retStart),collapse='') 
