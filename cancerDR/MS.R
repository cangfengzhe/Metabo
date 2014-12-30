library(stringr)
library(data.table)
library(devtools)
library(splitstackshape)

# 导入数据----
rawMs <- read.csv('data/MS.csv', stringsAsFactors = F) 

# 数据处理----
# 对单元格中的多个字符串进行分割
splitMs <-rawMs %>% 
          cSplit('cellLines', "|", 'long') %>%
            cSplit('disease', "|", 'long') %>%
             cSplit('drug', "|", 'long') %>%
                cSplit('ms', "|", 'long')
 
# from data.table to data.frame
splitMs <- as.data.frame(splitMs) 

# trim the string
procMs <- sapply(1:ncol(splitMs), function(x){
  splitMs$x <- as.character(splitMs[,x])
  str_trim(splitMs[,x])
})

# View(procMs)
colnames(procMs) <- colnames(splitMs)
attr(procMs,'note') <- '经过处理的MS数据，可以进行其他数据库的匹配'

# export data to local file
write.csv(procMs, file='./data/procMs.csv')


# link to otherDB----
procMs <- tbl_df(as.data.frame(procMs))
drugbank <- tbl_df(drugbank)
ms2DB <- left_join(procMs, drugbank, c('drug'='Name'), copy = T) 

