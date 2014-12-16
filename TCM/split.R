# id以逗号分割，计算数盐�<U+3E65>
library(RSQLite)
db <- dbDriver("SQLite")
con <- dbConnect(db, "sqlite1124.db")
str = result[, 4]
splitFun <- function(str) {
    
    listOut <- strsplit(str, ",")
    listOut <- as.data.frame(listOut[[1]])
    dbWriteTable(con, "listOut", listOut, append = T)
    
}

sapply(1:nrow(result), function(ii) {
    
    sapply(result[ii, 4], splitFun)
    print(ii)
    
})
aa <- dbGetQuery(con, "select  * from listOut")
nrow(aa) 
