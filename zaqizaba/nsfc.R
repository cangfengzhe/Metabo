library(RMySQL)
setwd('tcm_xx/')
options(stringsAsFactors =FALSE)
con = dbConnect(drv=RMySQL::MySQL(), user='root', password='', dbname='test')
nsfc <- dbReadTable(con, 'nsfc')
nsfc[1,]
aa <- read_csv('test.csv')

