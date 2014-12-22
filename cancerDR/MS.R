
rawMs <- read.csv('MS.csv', stringsAsFactors = F) 

names(rawMs)
attributes(rawMs)


process <- function(ii, colnum){
  
  
  out <- strsplit(data[ii,colnum],'\\|')[[1]]

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
