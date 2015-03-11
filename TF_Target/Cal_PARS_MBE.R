# data from GSE50676, calulate the PARS score
library(dplyr)
library(sqldf)
# GM12892, GM12878， GM12891
V1 <- read.delim("/Users/lipidong/work/protein bundunce/data/PARS_GEO_Data/GSM1226160_norm.GM12891_V1.tab", 
                 stringsAsFactors = F, header = F)

S1 <- read.delim("/Users/lipidong/work/protein bundunce/data/PARS_GEO_Data/GSM1226159_norm.GM12891_S1.tab", 
                 stringsAsFactors = F, header = F)

S1_V1 <- sqldf("select * from S1 inner join V1 on S1.V1=V1.V1")
rm(S1,V1)
gc()
S1_V1 <- S1_V1[, c(1, 2, 4)]
colnames(S1_V1) <- c("name", "S1", "V1")
s1_data <- strsplit(S1_V1[, 2], ";")
v1_data <- strsplit(S1_V1[, 3], ";")
pars = data.frame()  # save the mean
sapply(1:nrow(S1_V1), function(ii) {
  
  s1 <- as.numeric(s1_data[[ii]])
  v1 <- as.numeric(v1_data[[ii]])
  len <- length(s1)
  
  pars_base <- sapply(1: len, function(jj) {
    (v1[jj] + 1)/(s1[jj] + 1)
  })
  
  pars[ii, 1] <<- S1_V1[ii, 1]
  pars[ii, 2] <<- len
  # pars[ii,2] <<- paste(pars_base, collapse = ';')
  pars[ii, 3] <<- mean(pars_base)
  # when v1+s1 > 0
  pos <- which((v1+s1)>0)
  if(length(pos)>0){
    v1_mt0 <- v1[pos]
    s1_mt0 <- s1[pos]
    pars_base_mt0 <- sapply(1: length(pos), function(kk) {
      (v1_mt0[kk] + 1)/(s1_mt0[kk] + 1)
    })
    pars[ii,4] <<- mean(pars_base_mt0)
  }
 else{
   pars[ii,4] <<- 0
 }
  print(ii)
})
# mappding id

write.csv(pars, file = './data/pars_GM12891_MBE.csv')

library(org.Hs.eg.db)

gene2ref <- as.data.frame(org.Hs.egREFSEQ)
gene2name <- as.data.frame(org.Hs.egGENENAME)
pars_id <- sqldf("select pars.*,gene2name.gene_id, gene2name.gene_name  from pars left join gene2ref on pars.V1 = gene2ref.accession   left join gene2name on gene2name.gene_id = gene2ref.gene_id")
pars_id$type <- NA
pars_id$type[substr(pars_id$V1, 1, 2) == "NM"] = "mRNA"
pars_id$type[substr(pars_id$V1, 1, 2) == "NR"] = "non-coding RNA"
pars_id = pars_id[, c(1, 5, 2:4)]
pars_GM12892 <- pars_id
# save(pars_GM12878, file = './data/homo_pars.rdata') 
