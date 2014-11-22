# 远程操作 RJL‘s Rstudio，PostgreSQL
rm(list = ls())
library(RMySQL)
# setwd('~/rworkbench')
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host = "192.168.137.33", port = "5432", user = "postgres", 
    dbname = "dgidb")

`?`(`?`(RMySQL))


allTables <- dbListTables(con)
conMysql <- dbConnect(dbDriver("MySQL"), dbname = "PostgreSQL", user = "root", 
    password = "sql082508")
allTableMysql = dbListTables(conMysql)  #查询所有表

for (ii in 1:length(allTables)) {
    # readStr=c(allTables[ii],'= dbGetQuery(con,'select * from
    # ',allTables[ii],'')') #读取数据库 readStr1=paste(readStr,collapse='')
    # eval(parse(text=readStr1))
    writeStr = c("dbWriteTable(conMysql,'", allTables[ii], "',", allTables[ii], 
        ")")
    writeStr1 = paste(writeStr, collapse = "")
    writeStr1
    eval(parse(text = writeStr1))
}
pkg <- as.data.frame(library()$results[, 1])
aa <- matrix(aa <- NA, 105, 1)
aa[1:105] <- "install.packages('"
ii = 1
str <- matrix(NA, 105, 1)
for (ii in 1:105) {
    str[ii] <- paste(c(aa[ii, 1], as.character(pkg[ii, 1]), "')"), collapse = "")
}
pkg[2, 1]
str <- as.data.frame(str)
str <- paste(aa, pkg[1:105, 1], "')")
parse(text = str)
rm
str
str <- as.data.frame(str)
library()
.Library.site

aa <- cbind.data.frame(aa, pkg)
aa
cc <- matrix(NA, 105, 1)
cc[1:105] <- "')"
colnames(cc) <- c("aa", "bb", "cc")
cc <- cbind.data.frame(aa, cc)
library(sqldf) 
