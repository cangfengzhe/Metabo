# mapping all kind of data pars ---- data from
# http://genie.weizmann.ac.il/pubs/PARS10/
pars <- read.csv("/Users/lipidong/work/protein bundunce/data/pars.csv", 
    stringsAsFactors = F)
pars <- pars[, c(2, 5)]

# protein abundance ---- from http://pax-db.org/
protein_abun <- read.delim("/Users/lipidong/work/protein bundunce/data/protein abundance.txt", 
    stringsAsFactors = F)
protein_abun <- protein_abun[, c(2, 3)]

# mRNA fold free energy ---- from all sequence, which data from
# http://genie.weizmann.ac.il/pubs/PARS10/, by using matlab
# rnafold method
mf_all_seq <- read.csv("/Users/lipidong/work/protein bundunce/data/mF_all_seq.csv", 
    stringsAsFactors = F)
mf_all_seq <- mf_all_seq[, 2:3]

# mRNA half lives ---- from Global Analysis of mRNA Isoform
# Half-Lives Reveals Stabilizing and Destabilizing Elements in
# Yeast
mRNA_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/mRNA_halflives.csv", 
    stringsAsFactors = F)

# protein half lives ---- from Global Proteome Turnover
# Analyses of the Yeasts S. cerevisiae and S. pombe
protein_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/protein_halflives.csv", 
    stringsAsFactors = F)
protein_hl <- protein_hl[, 1:3]

# ppi ---- data from BIOGRID
ppi <- read.delim("/Users/lipidong/work/protein bundunce/data/PPI.txt", 
    stringsAsFactors = F)
ppi <- ppi[, 6:7]

# TF-target ---- data from YEASTRACT; http:// www.yeastract.com
tf_target <- read.csv("/Users/lipidong/work/protein bundunce/data/TF_target.csv", 
    stringsAsFactors = F)

# SGD name ---- from SGD database
library("org.Sc.sgd.db")
SGD <- as.data.frame(org.Sc.sgdGENENAME)
colnames(SGD) <- c("sys_name", "std_name")
# mapping to SGD id

library(sqldf)
pars_sgd <- sqldf("select SGD.sys_name, pars.* from pars left join SGD on pars.name = SGD.sys_name or pars.name=std_name")
pars_sgd[which(is.na(pars_sgd[, 1]) == T), 2]
save.image("./data/all_data.rdata") 
