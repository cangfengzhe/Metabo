
library(stringr)


library(RSQLite)
data <- read.csv('miRNA.csv', stringsAsFactors =F)
# View(data)
con <- dbConnect(dbDriver('SQLite'),'miRNAlit.db')
dbDisconnect(con)
dbGetQuery(con, 'drop table miRNA')


process <- function(ii, colnum){
  

  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii Ϊ?к?
  out <- str_trim(out)
  if(length(out)>0){
    df<- data.frame(id=data[ii,1], disease= out, cellLines= data[ii,3],
                    drug=data[ii,4], gene = data[ii,5],detail = data[ii,6],
                    row.names = NULL, stringsAsFactors = F 
                    )
    dbWriteTable(con, 'miRNA', df, append = T )
  }
  
  print(ii)
}

# ??ÿһ?н??д???

# ÿһ?д???ǰ????Ҫ?Ժ??? process ?????޸?

# ?ڶ???
process <- function(ii, colnum){
  
  #???н???,????һ?У?????????????
  
  # dataΪԭʼ????
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii Ϊ?к?
  out <- str_trim(out)
  if(length(out)>0){
    df<- data.frame(id=data[ii,1], disease= out, cellLines= data[ii,3],
                    drug=data[ii,4], gene = data[ii,5],detail = data[ii,6],
                    row.names = NULL, stringsAsFactors = F 
    )
    dbWriteTable(con, 'miRNA', df, append = T )
  }
  
  print(ii)
}

rowNum = 1:nrow(data)
sapply(rowNum, process, colnum=2)

# ??????
process <- function(ii, colnum){
  
  #???н???,????һ?У?????????????
  
  # dataΪԭʼ????
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii Ϊ?к?
  out <- str_trim(out)
  if(length(out)>0){
    df<- data.frame(id=data[ii,1], disease= data[ii,2], cellLines= out,
                    drug=data[ii,4], gene = data[ii,5],detail = data[ii,6],
                    row.names = NULL, stringsAsFactors = F 
    )
    dbWriteTable(con, 'miRNA', df, append = T )
  }
  
  print(ii)
}

data <- dbReadTable(con,'miRNA')
dbGetQuery(con, 'delete from miRNA')
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum=3)
View(data)

# ??4??

process <- function(ii, colnum){
  
  #???н???,????һ?У?????????????
  
  # dataΪԭʼ????
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii Ϊ?к?
  out <- str_trim(out)
  if(length(out)>0){
    df<- data.frame(id=data[ii,1], disease= data[ii,2], cellLines=data[ii,3],
                    drug=out, gene = data[ii,5],detail = data[ii,6],
                    row.names = NULL, stringsAsFactors = F 
    )
    dbWriteTable(con, 'miRNA', df, append = T )
  }
  
  print(ii)
}

data <- dbReadTable(con,'miRNA')
dbGetQuery(con, 'delete from miRNA')
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum=4)
data <- dbReadTable(con,'miRNA')
View(data)

# ??5??

process <- function(ii, colnum){
  
  #???н???,????һ?У?????????????
  
  # dataΪԭʼ????
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii Ϊ?к?
  out <- str_trim(out)
  if(length(out)>0){
    df<- data.frame(id=data[ii,1], disease= data[ii,2], cellLines=data[ii,3],
                    drug=data[ii,4], gene = out,detail = data[ii,6],
                    row.names = NULL, stringsAsFactors = F 
    )
    dbWriteTable(con, 'miRNA', df, append = T )
  }
  
  print(ii)
}


data <- dbReadTable(con,'miRNA')
dbGetQuery(con, 'delete from miRNA')
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum=5)
data <- dbReadTable(con,'miRNA')
View(data)
#
# ??5??
data <- dbReadTable(con,'miRNA')
dbGetQuery(con, 'delete from miRNA')
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum=5)
data <- dbReadTable(con,'miRNA')
View(data)
#

miRNALitData <- data
save(miRNALitData, file = 'miRNA.rdata')
write.csv(miRNALitData, file = 'miRNALit.csv', fileEncoding = "utf8")
