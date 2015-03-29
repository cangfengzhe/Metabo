liter <- matrix(NA, 1, 8)
result <- wei
for (ii in 1:336) {
    urlStr <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", 
        result[ii, 4], "&retmode=xml&rettype=abstract", collapse = "")
    tryCatch({
        text <- pubmedText(urlStr)
        nn <- nrow(text)
        if (nn != 0) {
            tmp <- matrix(NA, nn, 8)
            text <- as.matrix(text)
            tmp[1:nn, 1:3] <- result[rep(ii, nn), 1:3]
            tmp[1:nn, 4:8] <- text[c(1:nn), 1:5]
            
            
            liter <- rbind(liter, tmp)
            print(ii)
        }
    }, error = function(e) {
        print("chucuole")
    })
    
    
} 
