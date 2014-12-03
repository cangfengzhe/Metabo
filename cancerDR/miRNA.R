#处理文献中得到的miRna的数据 

library(stringr)


library(RSQLite)
data <- read.csv('miRNA.csv', stringsAsFactors =F)
# View(data)
con <- dbConnect(dbDriver('SQLite'),'miRNAlit.db')
dbDisconnect(con)
dbGetQuery(con, 'drop table miRNA')


process <- function(ii, colnum){
  
  #逐列进行,拆分一列，与其他列组合
  
  # data为原始数据
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii 为行号
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

# 对每一列进行处理

# 每一列处理前，需要对函数 process 进行修改

# 第二列
process <- function(ii, colnum){
  
  #逐列进行,拆分一列，与其他列组合
  
  # data为原始数据
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii 为行号
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

# 第三列
process <- function(ii, colnum){
  
  #逐列进行,拆分一列，与其他列组合
  
  # data为原始数据
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii 为行号
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

# 第4列

process <- function(ii, colnum){
  
  #逐列进行,拆分一列，与其他列组合
  
  # data为原始数据
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii 为行号
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

# 第5列

process <- function(ii, colnum){
  
  #逐列进行,拆分一列，与其他列组合
  
  # data为原始数据
  out <- strsplit(data[ii,colnum],'\\|')[[1]]
  # ii 为行号
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
# 第5列
data <- dbReadTable(con,'miRNA')
dbGetQuery(con, 'delete from miRNA')
rowNum = 1:nrow(data)
sapply(rowNum, process, colnum=5)
data <- dbReadTable(con,'miRNA')
View(data)
#

miRNALitData <- data
save(miRNALitData, file = 'miRNA.rdata')
write.csv(miRNALitData, file = 'miRNALit.csv')
