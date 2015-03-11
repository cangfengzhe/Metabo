# process miRna target
# miRTarBase database ----
expTar <- read.csv('./mirTarget//miRTarBase_experiment.csv', stringsAsFactors = F)
View(expTar)

# microRNA database ----
microRna <- read.delim('./mirTarget//MICRORNA.ORG_mirTarget_ 2010/hg19_predictions_S_0_aug2010.txt',
stringsAsFactors = F)
nrow(microRna)

microRna_S0 <- microRna

microRna_SC <- read.delim('./mirTarget//MICRORNA.ORG_mirTarget_ 2010/hg19_predictions_S_C_aug2010.txt',
stringsAsFactors = F)
View(microRna_S0)
View(microRna_SC)
microRna <- rbind(microRna_S0[,2:4],microRna_SC[,2:4])
rm(c(microRna_S0,microRna_SC))
rm(c('microRna_S0','microRna_SC'))
rm(list=c('microRna_S0','microRna_SC'))
gc()

# miRDB database----
miRDB <- read.table('./mirTarget/miRDB_prediction_result_2014.txt/miRDB_v5.0_prediction_result.txt',
stringsAsFactors = F)

# TARGETSCAN database ----
targetScan <- read.delim('./mirTarget/TARGETSCAN-VERT_Predicted_Targets_Info_12.txt/Predicted_Targets_Info.txt')
View(targetScan)
targetScan_filter <- targetScan  %>% filter(Species.ID=='9606')
targetScan_filter <- targetScan_filter[,1:4]
familyInfo <- read.delim('./mirTarget//TARGETSCAN-VERT_Predicted_Targets_Info_12.txt/miR_Family_Info.txt')
familyInfo_filter <- filter(familyInfo, Species.ID=='9606')
targetScan_mir <- left_join(targetScan_filter, familyInfo_filter, 
                            by = c('miR.family','miR.Family'))
nrow(familyInfo_filter)
library(sqldf)
colnames(familyInfo_filter)[1] <- 'mirFamily'
colnames(targetScan_filter)[1] <- 'mirFamily'
targetScan_mir <- sqldf('select * from targetScan_filter
                        left join familyInfo_filter on
                        familyInfo_filter.mirFamily = targetScan_filter.mirFamily')
targetScan_mir2 <- targetScan_mir[,c(1:4,8,11)]

# nucleotide accession----
acc_nm <- targetScan_mir2[,2:4]
acc_nm <- unique(acc_nm)
nrow(acc_nm) #19074
save.image()

# left join miRDB
miRDB2 <- filter(miRDB, grepl('hsa', V1))
colnames(acc_nm)[3] <- 'Trancsript_id'

miRDB2  <- sqldf('select * from miRDB2 left join acc_nm
                  on miRDB2.V2=acc_nm.Trancsript_id')
# effect is not good to left join the acc_mn
# read the txt gene2accession
library(ff)
aa <- read.table.ffdf( 'D:/deskTop/aa.txt',  fill = TRUE)

gene2acc <- read.delim.ffdf(x=NULL,'F:/迅雷下载/gene2accession/gene2accession',
                            
                            skip = 11460846,
                            transFUN=function(x){
                              tryCatch({
                               x[which(x[,1]=='9606'),c(1,2,6,8,16)]
                              }, error = function(e){
                                 aa[2, c(1,2,6,8,16)]
                              })
                                
                            },VERBOSE = T
                            )

library(dplyr)
miRDB2 <- filter(miRDB, grepl('hsa', V1))
write.csv(miRDB2, file = './mirTarget/miRDB2.csv')
write.table(unique(miRDB2[,2]), file = './mirTarget/miRDB2.txt', col.names = F, row.names = F, quote = F)

length(unique(miRDB2[,2]))
source("http://bioconductor.org/workflows.R")
workflowInstall("annotation")
biocLite('org.Hs.eg.db')
biocLite('org.Hs.egREFSEQ')
library(org.Hs.eg.db)
View(org.Hs.egREFSEQ)
gene2ref <- as.data.frame(org.Hs.egREFSEQ)

library(sqldf)
miRDB3 <- sqldf('select * from miRDB2 left join 
                gene2ref on miRDB2.V2=gene2ref.accession')
View(miRDB3)
View(org.Hs.egSYMBOL)
geneName <- as.data.frame(org.Hs.egSYMBOL)
miRDB3 <- miRDB3[,c(1:3,7,8)]
miRDB <- sqldf('select * from miRDB3 left join geneName 
               on miRDB3.gene_id= geneName.gene_id')
miRDB <- miRDB[,c(1:3,6,7)]



# mapping miRBase database id & tidy the data ---
miRBase <- read.csv('./mirTarget/miR_aliases.csv')

miRDB2 <- sqldf('select * from miRDB left join miRBase
                on miRDB.V1 = miRBase.V2')
miRDB3 <- miRDB2[,c(1:5,7)]
miRDB <- miRDB3[,c(6,1,5,6)] 
miRDB2 <- sqldf('select * from miRDB left join geneName on
                miRDB.symbol = geneName.symbol')
miRDB <- miRDB2[,c(1,2,5,3)]
names(miRDB) <- c('mirdb_mirID','mirdb_mirName','mirdb_geneID', 'mirdb_geneName')

# microRna
microRna2 <- sqldf('select * from microRna left join miRBase 
                   on microRna.mirna_name= miRBase.V2')
microRna <- microRna2[,c(5,1:3)]
names(microRna) <- c('microrna_mirID','microrna_mirName','microrna_geneID', 'microrna_geneName')
# targetScan
targetScan <- targetScan_mir2[,c(6,5,2,3)]
names(targetScan) <- c('tarscan_mirID','tarscan_mirName','tarscan_geneID', 'tarscan_geneName')
#  expTar expriment
expTar <- expTar[,c(1,2,5,4)]
names(expTar) <- c('exptar_mirID','exptar_mirName','exptar_geneID', 'exptar_geneName')
head(expTar)
save.image('miRNATarget_tidy.rdata')

# intersect
miRDB2microRna <- sqldf('select microRna.* from miRDB inner join
                        microRna on miRDB.mirdb_geneID=microRna.microrna_geneID and miRDB.mirdb_mirID = microRna.microrna_mirID')


miRDB2microRna <- sqldf('select microRna.* from miRDB inner join
                        microRna on miRDB.mirdb_geneID=microRna.microrna_geneID and miRDB.mirdb_mirID = microRna.microrna_mirID
                        inner join targetScan on miRDB.mirdb_geneID=targetScan.tarscan_geneID and miRDB.mirdb_mirID = targetScan.tarscan_mirID')

mir3 <- sqldf('select targetScan.* from targetScan inner join miRDB2microRna
              on targetScan.tarscan_mirID=miRDB2microRna.microrna_mirID
              and targetScan.tarscan_geneID=miRDB2microRna.microrna_geneID')

mir4 <- sqldf('select microRna.* from miRDB inner join
                        microRna on miRDB.mirdb_geneID=microRna.microrna_geneID and miRDB.mirdb_mirID = microRna.microrna_mirID
                        inner join targetScan on miRDB.mirdb_geneID=targetScan.tarscan_geneID and miRDB.mirdb_mirID = targetScan.tarscan_mirID')

# mir3==mir4 , 1129360 条, unique 之后mir5  60158
mir5 <- unique(mir4)
View(head(mir5))
nrow(expTar)
length(unique(expTar[,2]))
