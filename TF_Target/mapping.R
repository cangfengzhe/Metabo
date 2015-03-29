# mapping all kind of data pars ---- SGD name ---- from SGD
# database
SGD <- read.csv("/Users/lipidong/work/protein bundunce/data/sgd_gene_mapping.csv", 
    stringsAsFactors = F)
SGD <- cSplit(SGD, 4, sep = "|", direction = "long")
library(sqldf)


# data from http://genie.weizmann.ac.il/pubs/PARS10/
pars <- read.csv("/Users/lipidong/work/protein bundunce/data/pars.csv", 
    stringsAsFactors = F)
pars <- pars[, c(2, 5)]
pars_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, pars.* from pars left join SGD on pars.name = SGD.sys_name or pars.name=SGD.std_name")

# protein abundance ---- from http://pax-db.org/
protein_abun <- read.delim("/Users/lipidong/work/protein bundunce/data/protein abundance.txt", 
    stringsAsFactors = F)
protein_abun <- protein_abun[, c(2, 3)]
colnames(protein_abun)[1] <- "name"
protein_abun_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, protein_abun.* from protein_abun left join SGD on protein_abun.name = SGD.sys_name or protein_abun.name=SGD.std_name")

# mRNA fold free energy ---- from all sequence, which data from
# http://genie.weizmann.ac.il/pubs/PARS10/, by using matlab
# rnafold method
mf_all_seq <- read.csv("/Users/lipidong/work/protein bundunce/data/mF_all_seq.csv", 
    stringsAsFactors = F)
mf_all_seq <- mf_all_seq[, 2:3]
mf_all_seq_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, mf_all_seq.* from mf_all_seq left join SGD on mf_all_seq.name = SGD.sys_name or mf_all_seq.name=SGD.std_name")
mf_cds <- read.csv("./data/sce_mF_cds_out.csv", stringsAsFactors = F)
mf_cds_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, mf_cds.* from mf_cds left join SGD on mf_cds.name = SGD.sys_name or mf_cds.name=SGD.std_name")
mf_utr5 <- read.csv("./data/sce_mF_utr5_out.csv", stringsAsFactors = F)
mf_utr5_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, mf_utr5.* from mf_utr5 left join SGD on mf_utr5.name = SGD.sys_name or mf_utr5.name=SGD.std_name")

mf_utr3 <- read.csv("./data/sce_mF_utr3_out.csv", stringsAsFactors = F)
mf_utr3_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, mf_utr3.* from mf_utr3 left join SGD on mf_utr3.name = SGD.sys_name or mf_utr3.name=SGD.std_name")

# mRNA half lives ---- from Global Analysis of mRNA Isoform
# Half-Lives Reveals Stabilizing and Destabilizing Elements in
# Yeast
mRNA_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/mRNA_halflives.csv", 
    stringsAsFactors = F)
colnames(mRNA_hl)[1] <- "name"
mRNA_hl_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, mRNA_hl.* from mRNA_hl left join SGD on mRNA_hl.name = SGD.sys_name or mRNA_hl.name=SGD.std_name")
colnames(mRNA_hl_sgd)[4] <- "mRNA_halflife"
# protein half lives ---- from Global Proteome Turnover
# Analyses of the Yeasts S. cerevisiae and S. pombe
protein_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/protein_halflives.csv", 
    stringsAsFactors = F)
protein_hl <- protein_hl[, 1:3]
# split the first column
library(splitstackshape)
protein_hl <- cSplit(protein_hl, splitCols = 1, sep = ";", direction = "long")
colnames(protein_hl) <- c("name", "hl_min", "hl_hour")
protein_hl_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, protein_hl.* from protein_hl left join SGD on protein_hl.name = SGD.sys_name or protein_hl.name=SGD.std_name")


# ppi ---- data from BIOGRID
ppi <- read.delim("/Users/lipidong/work/protein bundunce/data/PPI.txt", 
    stringsAsFactors = F)
ppi <- ppi[, 6:7]

# TF-target ---- data from YEASTRACT; http:// www.yeastract.com
tf_target <- read.csv("/Users/lipidong/work/protein bundunce/data/TF_target.csv", 
    stringsAsFactors = F)
target_list <- as.data.frame(unique(tf_target[, 2]))
colnames(target_list) <- "name"
# sqldf 采用sqlite数据库 大小写敏感
target_sgd <- sqldf("select distinct target_list.*, SGD.sgd_id, SGD.sys_name from target_list left join SGD on LOWER(target_list.name) = LOWER(SGD.sys_name) OR LOWER(target_list.name) = LOWER(SGD.std_name) ")
write.csv(target_sgd, file = "./data/target_sgd.csv")
# 人工匹配
target_sgd <- read.csv("./data/target_sgd.csv", stringsAsFactors = F)
tf_target_sgd <- sqldf("select distinct tf_target.*, target_sgd.sgd_id from tf_target left join target_sgd on target_sgd.name = tf_target.target")



save.image("./data/all_data.rdata")
all_data <- sqldf("select SGD.*, pars_sgd.mean as pars, protein_abun_sgd.abundance as protein_abundance from SGD left join pars_sgd on SGD.sgd_id = pars_sgd.sgd_id\n                  left join protein_abun_sgd on protein_abun_sgd.sgd_id = SGD.sgd_id")

all_data <- sqldf("select SGD.")
 
