library(RMySQL)
conn <- dbConnect(drv = dbDriver('MySQL'), dbname = 'cancerdr',host = 'localhost', 
                  user = 'root', password = '')
term <- read.csv('./data/cell_name_revise.csv', stringsAsFactors = F)

sapply(1:nrow(term), function(x){
  sql <- paste(c('update ', term[x,1], ' set disease_id=', 
          term[x,4], ' where disease_id=', term[x,2], 
          ' and cell_id =', term[x,3], ';'), collapse = '')
  flag <- dbGetQuery(conn, sql);
  print(flag)
})
x=3
x=1
