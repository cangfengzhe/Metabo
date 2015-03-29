entrez2kegg <- read.table("./mirTarget/entriz2kegg.txt", stringsAsFactors = F)
View(entrez2kegg)
mirPathway$geneID
geneIDList <- strsplit(mirPathway$geneID, "/")
length(geneIDList)
sapply(1:nrow(mirPathway), function(ii) {
    keggID <- entrez2kegg[match(geneIDList[[ii]], entrez2kegg[, 
        1]), 2]
})


View(entrez2kegg) 
