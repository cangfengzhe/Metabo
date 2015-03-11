# process the methylation data related to the drug resistance 
# from literature
# 2014
library(splitstackshape)
library(stringr)
# import data ----
rawMeth <- read.csv('data/methylation.csv', stringsAsFactors = F)
# View(meth)

# process the data ----
# 单元格分割
splitMeth <-rawMeth %>% 
  cSplit('cellLines', "|", 'long') %>%
  cSplit('disease', "|", 'long') %>%
  cSplit('drug', "|", 'long') %>%
  cSplit('gene', "|", 'long')

# from data.table to data.frame
splitMeth <- as.data.frame(splitMeth) 

# trim the string
procMeth <- sapply(1:ncol(splitMeth), function(x){
  splitMeth[,x] <- as.character(splitMeth[,x])
  str_trim(splitMeth[,x])
})
View(procMeth)

# export data to local file----
write.csv(procMeth, file='data/procMeth.csv')

# link to other database in excel





