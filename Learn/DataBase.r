library(DBI)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host = "192.168.137.33", port = "5432", user = "postgres", dbname = "dgidb")

aa = dbListTables(con)
class(aa)
typeof(aa)

`?`(RMySQL)
rm(list = ls())
tab = read.csv("C:\\Users\\Administrator\\Desktop\\datafile.tsv")
`?`(read.csv) 
