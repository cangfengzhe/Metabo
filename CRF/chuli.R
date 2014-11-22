geneDataTmp <- proteinDataTmp
geneDataTmp[, 1] <- as.character(geneDataTmp[, 1])
rowNum <- nrow(geneDataTmp)
pmid <- ""
protein0 <- ""
DNA <- ""
DNA0 <- ""
result <- data.frame(pmid, protein, DNA)
result0 <- data.frame(pmid, protein, DNA)

for (ii in 1:rowNum) {
    if (geneDataTmp[ii, 3] == "A") {
        print(ii)
        pmid <- substr(geneDataTmp[ii, 1], 4, nchar(geneDataTmp[ii, 1]))
        for (jj in (ii + 1):rowNum) {
            if (geneDataTmp[jj, 3] == "B-protein") {
                protein <- geneDataTmp[jj, 1]
                for (kk in (jj + 1):rowNum) {
                  if (geneDataTmp[kk, 3] == "I-protein") {
                    protein <- paste(protein, geneDataTmp[kk, 1])
                  } else {
                    protein0 <- rbind(protein0, protein)
                    
                    break
                  }
                }
            }
            
            # 
            
            if (geneDataTmp[jj, 3] == "A") {
                break
            }
        }
        for (jj in (ii + 1):rowNum) {
            if (geneDataTmp[jj, 3] == "B-DNA") {
                DNA <- geneDataTmp[jj, 1]
                for (kk in (jj + 1):rowNum) {
                  if (geneDataTmp[kk, 3] == "I-DNA") {
                    DNA <- paste(DNA, geneDataTmp[kk, 1])
                  } else {
                    DNA0 <- rbind(DNA0, DNA)
                    
                    break
                  }
                }
            }
            
            # 
            
            if (geneDataTmp[jj, 3] == "A") {
                break
            }
        }
        
        
        DNA0 <- unique(DNA0)
        DNA <- paste(DNA0, collapse = "|")
        protein0 <- unique(protein0)
        protein <- paste(protein0, collapse = "|")
        result <- data.frame(pmid, protein, DNA)
        result0 <- rbind(result0, result)
        pmid <- ""
        protein0 <- ""
        protein <- ""
        DNA <- ""
        DNA0 <- ""
    }
    
} 
