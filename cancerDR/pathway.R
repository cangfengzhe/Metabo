source("http://bioconductor.org/biocLite.R")
biocLite("clusterProfiler")
biocLite('pathview')
library(sqldf)
library(clusterProfiler)
load('./data_0213.RData')
colnames(miRDB) <- c('mirID', 'mirName', 'geneID', 'geneName')
colnames(microRna)<- c('mirID', 'mirName', 'geneID', 'geneName')
colnames(targetScan) <- c('mirID', 'mirName', 'geneID', 'geneName')
miRDB2microRna <- sqldf('select  miRDB.mirID, miRDB.geneID from miRDB inner join microRna on
              miRDB.mirID=microRna.mirID and miRDB.geneID=microRna.geneID')

miRDB2targetScan <- sqldf('select  miRDB.mirID, miRDB.geneID from miRDB inner join targetScan on
              miRDB.mirID=targetScan.mirID and miRDB.geneID=targetScan.geneID')

microRna2targetScan <- sqldf('select  microRna.mirID, microRna.geneID from microRna inner join targetScan on
              microRna.mirID=targetScan.mirID and microRna.geneID=targetScan.geneID')

miRDB2microRna$mirdb <- 1
miRDB2microRna$microrna <- 1
miRDB2microRna$targetscan <- 0
colnames(miRDB2microRna)

miRDB2targetScan$mirdb <- 1
miRDB2targetScan$targetscan <- 1
miRDB2targetScan$microrna <- 0
miRDB2targetScan <- miRDB2targetScan[,c(1,2,3,5,4)]
colnames(miRDB2targetScan)


microRna2targetScan$microrna <- 1
microRna2targetScan$targetscan <- 1
microRna2targetScan$mirdb <- 0
microRna2targetScan <- microRna2targetScan[,c(1,2,4,5,3)]
colnames(microRna2targetScan)

mir3 <- unique(rbind(miRDB2microRna,miRDB2targetScan, microRna2targetScan))
mir3_unique <- unique(mir3[,1:2]) # result for predict

# add expriement
colnames(expTar) <- c('mirID', 'mirName', 'geneID', 'geneName')
mirAll <- unique(rbind(mir3_unique, expTar[,c(1,3)]) )
nrow(mirAll)
save(miRDB2microRna,miRDB2targetScan, microRna2targetScan, mirAll, file='mir2target.rdata')

## enrichment start

drug2mir <- read.csv('./mirTarget/drug_mir.csv', stringsAsFactors = F)
head(drug_mir)
drug2tar <- sqldf('select drug2mir.*, mirAll.geneID from drug2mir left join mirAll on
                  drug2mir.miRbase_id=mirAll.mirID')
head(drug2tar)


## enrichment
library(dplyr)
library(RSQLite)
conn <- dbConnect(SQLite(), './mirTarget/sqlite.db')
drug <- unique(drug2tar[,1:2])
errorOut <- NA
jj <- 0
sapply(1:nrow(drug), function(ii){
  tryCatch({
    geneID <- drug2tar[which(drug2tar[,2]==drug[ii,2]),4]
#    geneTable <- as.data.frame(table(geneID), stringsAsFactors = F)
#     geneID <- na.omit(unique(geneTable[which(geneTable[,2]>1 ),1]))
  geneID <- na.omit(unique(geneID))   
prepathway <- enrichKEGG(gene=geneID,pAdjustMethod="bonferroni",
                             pvalueCutoff = 0.05)
    pathway <- summary(prepathway)
    pathway$drugid <- drug[ii,2]
    dbWriteTable(conn, 'pathway', pathway, append = T)
    print(ii)
  }, error=function(e){
    print(e)
    jj <<- jj+1;
    errorOut[jj] <<- ii
  })
   
})

mirPathway <- dbReadTable(conn, 'pathway')

# meth pathway
drug2meth <- read.csv('./mirTarget/drug_meth.csv', stringsAsFactors = F)

drug <- unique(drug2meth[,1:2])
errorOut <- NA
jj <- 0
sapply(1:nrow(drug), function(ii){
  tryCatch({
    geneID <- drug2meth[which(drug2meth[,2]==drug[ii,2]),5]
    #    geneTable <- as.data.frame(table(geneID), stringsAsFactors = F)
    #     geneID <- na.omit(unique(geneTable[which(geneTable[,2]>1 ),1]))
    geneID <- na.omit(unique(geneID))  
    geneID <- geneID[geneID!='NULL']
    prepathway <- enrichKEGG(gene=geneID,
                             pAdjustMethod="bonferroni",
                             pvalueCutoff = 0.05)
    
    if( is.null(prepathway)==F ){
      pathway <- summary(prepathway)
      #     reactpathway <- enrichPathway(gene=geneID,
      #                   pAdjustMethod="bonferroni",
      #                   pvalueCutoff = 0.05)
      print(pathway)
      
      pathway$drugid <- drug[ii,2]
      dbWriteTable(conn, 'methpathway', pathway, append = T)
      print(ii)
    }

  }, error=function(e){
    print(e)
    jj <<- jj+1;
    errorOut[jj] <<- ii
  })
  
})


dbReadTable(conn, 'methpathway') %>%View()
mirPathway <- dbReadTable(conn, 'pathway')

write.csv(dbReadTable(conn, 'pathway'), file='./mirTarget/mirPathway.csv')

