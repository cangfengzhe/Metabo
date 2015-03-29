source("http://bioconductor.org/biocLite.R")
biocLite("RDAVIDWebService")

library(RDAVIDWebService)
david <- DAVIDWebService$new(email = "78956287@qq.com")
data(demoList1)
result <- addList(david, demoList1, idType = "AFFYMETRIX_3PRIME_IVT_ID", 
    listName = "demoList1", listType = "Gene")
setAnnotationCategories(david, c("GOTERM_BP_ALL", "GOTERM_MF_ALL", 
    "GOTERM_CC_ALL"))

termCluster <- getClusterReport(david, type = "Term")
getClusterReportFile(david, type = "Term", fileName = "termClusterReport1.tab") 
