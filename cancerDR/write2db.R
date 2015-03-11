library(dplyr)

fileList <- dir('./data/database/')
my_db <- src_mysql(dbname = 'dr',host = 'localhost', 
                   user = 'root', password = '')

copy_to(my_db, get(filename), name = filename)      


# write to the mysql by using RMySQL-----
conn <- dbConnect(drv = dbDriver('MySQL'), dbname = 'cancerdr',host = 'localhost', 
                  user = 'root', password = '')
sapply(fileList,function(x){
  filename <- substr(x,1,nchar(x)-4)
  assign(filename ,read.csv(paste(c('./data/database/', x), collapse = ''), stringsAsFactors = F))
         
  
  dbWriteTable(conn, filename, value = get(filename), row.names = F, append = T)
})


# set the field type -----
tableList <- dbListTables(conn)
x <- tableList[1]
sqlQuery <-paste(c('select * from ', x, ' PROCEDURE ANALYSE()'), collapse = '') 
result <- dbGetQuery(conn, sqlQuery)

aa  <-  read.csv('./data/database/Info_IC50.csv')

dbWriteTable(conn,name = 'info_ic50', value=aa, append = T, row.name = F)


       