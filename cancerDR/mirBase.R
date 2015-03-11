rawData <- readLines('./data/miRNA.dat/miRNA.dat')
View(rawData)
data1 <- rawData[grep('^ID\\b|^AC\\b|^DE\\b', rawData)]

ID <- data1[grep('^ID\\b', data1)]
AC <- data1[grep('^AC\\b', data1)]
DE <- data1[grep('^DE\\b', data1)]
# View(ID)
IDtmp <- strsplit(ID,split = '\\s+', perl=T)
ID1 <- do.call('rbind',IDtmp) 
View(ID1)
ACtmp <- strsplit(AC,split = '\\s+', perl=T)
AC1 <- do.call('rbind',ACtmp) 

DEtmp <- strsplit(DE,split = 'DE', perl=T)
DE1 <- do.call('rbind',DEtmp) 
View(DE1)
mirBase <- cbind(AC1[,2], ID1[,2], DE1[,2])
View(mirBase)
write.csv(mirBase, file='./data/mirbase.csv')

# 2015-02-11-new ----
mirBase <- read.delim('./mirTarget/miR_aliases.txt',
                      header = F)
# separate
library(splitstackshape)
library(dplyr)
mirBase_split <- cSplit(mirBase, 2, ';', direction = 'long')
hsaMir <- filter(mirBase_split, grepl('hsa', V2))
hsaMir_MIMA <- filter(hsaMir, grepl('MIMA', V1))

write.csv(hsaMir, file='./data/miR_aliases.csv')



