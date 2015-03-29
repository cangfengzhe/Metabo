# library(stringr) library(dplyr)
lib <- readLines("/Users/lipidong/Desktop/library.txt")
lib <- str_trim(lib)

text <- readLines("/Users/lipidong/Desktop/text.txt")
text <- str_trim(substr(text, start = 4, nchar(text)))
out <- data.frame()
blank <- NA
jj <- 0
kk <- 0
sapply(1:length(lib), function(ii) {
    print(ii)
    tmp <- text[grep(lib[ii], text)]
    if (length(tmp) > 0) {
        jj <<- jj + 1
        out[(kk + 1), 1] <<- jj
        out[(kk + 1), 2] <<- lib[ii]
        out[(kk + 2):(kk + length(tmp) + 1), 1] <<- NA
        out[(kk + 2):(kk + length(tmp) + 1), 2] <<- tmp
        kk <<- kk + length(tmp) + 1
        
        
    } else {
        blank <<- c(blank, lib[ii])
    }
    
})
write.csv(out, file = "./data/out.txt", quote = T, row.names = F, 
    fileEncoding = "UTF-8")

write.csv(blank, file = "./data/wu.txt", quote = F, fileEncoding = "UTF-8") 
