# data from GSE50676, calulate the PARS score
library(dplyr)
library(sqldf)
V1 <- read.delim("/Users/lipidong/work/protein bundunce/data/PARS_GEO_Data/GSM1226162_norm.GM12892_V1.tab", 
    stringsAsFactors = F, header = F)

S1 <- read.delim("/Users/lipidong/work/protein bundunce/data/PARS_GEO_Data/GSM1226161_norm.GM12892_S1.tab", 
    stringsAsFactors = F, header = F)
# V1 <- read.delim('/Users/lipidong/work/protein
# bundunce/data/PARS_GEO_Data/sce_V1.tab', stringsAsFactors =
# F, header = F) S1 <- read.delim('/Users/lipidong/work/protein
# bundunce/data/PARS_GEO_Data/sce_S1.tab', stringsAsFactors =
# F, header = F) Score <-
# read.delim('/Users/lipidong/work/protein
# bundunce/data/PARS_GEO_Data/sce_Score.tab', stringsAsFactors
# = F, header = F) S1 V1 合并
S1_V1 <- sqldf("select * from S1 inner join V1 on S1.V1=V1.V1")
S1_V1 <- S1_V1[, c(1, 2, 4)]
colnames(S1_V1) <- c("name", "S1", "V1")
s1_data <- strsplit(S1_V1[, 2], ";")
v1_data <- strsplit(S1_V1[, 3], ";")
pars = data.frame()  # save the mean
sapply(1:nrow(S1_V1), function(ii) {
    
    s1 <- as.numeric(s1_data[[ii]])
    v1 <- as.numeric(v1_data[[ii]])
    len <- length(s1)
    # first 2
    pars01 <- log2(v1[1] + 5) - log2(s1[1] + 5)
    pars02 <- log2(v1[2] + 5) - log2(s1[2] + 5)
    # last 2
    pars11 <- log2(v1[len] + 5) - log2(s1[len] + 5)
    pars12 <- log2(v1[len - 1] + 5) - log2(s1[len - 1] + 5)
    # from 3 to end-2
    pars_center <- sapply(3:(len - 2), function(jj) {
        log2(mean(v1[(jj - 2):(jj + 2)]) + 5) - log2(mean(s1[(jj - 
            2):(jj + 2)]) + 5)
    })
    pars_base <- c(pars01, pars02, pars_center, pars12, pars11)
    pars[ii, 1] <<- S1_V1[ii, 1]
    # pars[ii,2] <<- paste(pars_base, collapse = ';')
    pars[ii, 2] <<- mean(pars_base)
    print(ii)
})


# mappding id
library(org.Hs.eg.db)

gene2ref <- as.data.frame(org.Hs.egREFSEQ)
gene2name <- as.data.frame(org.Hs.egGENENAME)
pars_id <- sqldf("select pars.V1,gene2name.gene_id, gene2name.gene_name, pars.V2 as score from pars left join gene2ref\n                 on pars.V1 = gene2ref.accession\n                 left join gene2name on gene2name.gene_id = gene2ref.gene_id")
pars_id$type <- NA
pars_id$type[substr(pars_id$V1, 1, 2) == "NM"] = "mRNA"
pars_id$type[substr(pars_id$V1, 1, 2) == "NR"] = "non-coding RNA"
pars_id = pars_id[, c(1, 5, 2:4)]
pars_GM12892 <- pars_id
# save(pars_GM12878, file = './data/homo_pars.rdata') 
