# covert the text to a column of word, meanwhile transforming
# the stem of words by using the package of tm
library(XML)
library(tm)
cbtxt1 = NA
n = 0
error = NA
ii = 17615
source("downLoadText.R")
for (ii in 17615:nrow(Pmid)) {
    tryCatch({
        txt <- downLoadText(Pmid[ii])  #use my own function
        txtCol <- MC_tokenizer(txt)  #get the column of word
        txtCol <- as.matrix(txtCol)
        cbtxt1 <- rbind(cbtxt1, txtCol)
        cbtxt1 <- unique(cbtxt1)
        print(ii)
    }, error = function(e) {
        n = n + 1
        error[n] <- Pmid[ii]
    })
}

cbtxt <- rbind(cbtxt, cbtxt1)
cbtxt <- unique(cbtxt)
length(cbtxt)
cbtxt
error
save(cbtxt, Pmid, file = "cbtxt.rdata") 
