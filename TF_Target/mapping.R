# mapping all kind of data pars ----
# SGD name ---- from SGD database
SGD <- read.csv('/Users/lipidong/work/protein\ bundunce/data/sgd_gene_mapping.csv', stringsAsFactors = F)

library(sqldf)


# data from  http://genie.weizmann.ac.il/pubs/PARS10/
pars <- read.csv("/Users/lipidong/work/protein bundunce/data/pars.csv", 
    stringsAsFactors = F)
pars <- pars[, c(2, 5)]
pars_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, pars.* from pars left join SGD on pars.name = SGD.sys_name or pars.name=SGD.std_name")

# protein abundance ---- from http://pax-db.org/
protein_abun <- read.delim("/Users/lipidong/work/protein bundunce/data/protein abundance.txt", 
    stringsAsFactors = F)
protein_abun <- protein_abun[, c(2, 3)]
colnames(protein_abun)[1] <- 'name'
protein_abun_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, protein_abun.* from protein_abun left join SGD on protein_abun.name = SGD.sys_name or protein_abun.name=SGD.std_name")

# mRNA fold free energy ---- from all sequence, which data from
# http://genie.weizmann.ac.il/pubs/PARS10/, by using matlab
# rnafold method
mf_all_seq <- read.csv("/Users/lipidong/work/protein bundunce/data/mF_all_seq.csv", 
    stringsAsFactors = F)
mf_all_seq <- mf_all_seq[, 2:3]
mf_all_seq_sgd <- sqldf('select SGD.sgd_id, SGD.sys_name, mf_all_seq.* from mf_all_seq left join SGD on mf_all_seq.name = SGD.sys_name or mf_all_seq.name=SGD.std_name')
# mRNA half lives ---- from Global Analysis of mRNA Isoform
# Half-Lives Reveals Stabilizing and Destabilizing Elements in
# Yeast
mRNA_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/mRNA_halflives.csv", 
    stringsAsFactors = F)
colnames(mRNA_hl)[1] <- 'name'
mRNA_hl_sgd <- sqldf('select SGD.sgd_id, SGD.sys_name, mRNA_hl.* from mRNA_hl left join SGD on mRNA_hl.name = SGD.sys_name or mRNA_hl.name=SGD.std_name')
# protein half lives ---- from Global Proteome Turnover
# Analyses of the Yeasts S. cerevisiae and S. pombe
protein_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/protein_halflives.csv", 
    stringsAsFactors = F)
protein_hl <- protein_hl[, 1:3]
# split the first column
library(splitstackshape)
protein_hl <- cSplit(protein_hl, splitCols = 1, sep = ';', direction = "long")
# ppi ---- data from BIOGRID
ppi <- read.delim("/Users/lipidong/work/protein bundunce/data/PPI.txt", 
    stringsAsFactors = F)
ppi <- ppi[, 6:7]

# TF-target ---- 
# data from YEASTRACT; http:// www.yeastract.com
tf_target <- read.csv("/Users/lipidong/work/protein bundunce/data/TF_target.csv", 
    stringsAsFactors = F)


save.image('./data/all_data.rdata')



