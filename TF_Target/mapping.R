# mapping all kind of data pars ---- 
#SGD name ---- from SGD
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

# mRNA fold free energy ---- 
# from all sequence, which data from
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

# mRNA half lives ---- 
# from Global Analysis of mRNA Isoform
# Half-Lives Reveals Stabilizing and Destabilizing Elements in
# Yeast
mRNA_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/mRNA_halflives.csv", 
    stringsAsFactors = F)
colnames(mRNA_hl)[1] <- "name"
mRNA_hl_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, mRNA_hl.* from mRNA_hl left join SGD on mRNA_hl.name = SGD.sys_name or mRNA_hl.name=SGD.std_name")
colnames(mRNA_hl_sgd)[4] <- "mRNA_halflife"

# protein half lives ---- 
# from Global Proteome Turnover
# Analyses of the Yeasts S. cerevisiae and S. pombe
protein_hl <- read.csv("/Users/lipidong/work/protein bundunce/data/protein_halflives.csv", 
    stringsAsFactors = F)
protein_hl <- protein_hl[, 1:3]
# split the first column
library(splitstackshape)
protein_hl <- cSplit(protein_hl, splitCols = 1, sep = ";", direction = "long")
colnames(protein_hl) <- c("name", "hl_min", "hl_hour")
protein_hl_sgd <- sqldf("select SGD.sgd_id, SGD.sys_name, protein_hl.* from protein_hl left join SGD on protein_hl.name = SGD.sys_name or protein_hl.name=SGD.std_name")
protein_hl_sgd <- filter(protein_hl_sgd, hl_min != 'n.d.')
protein_hl_sgd$hl_min <- as.numeric(protein_hl_sgd$hl_min 
)
View(protein_hl_sgd)
protein_hl_sgd01 <- group_by(protein_hl_sgd, sgd_id) %>% 
  summarise(hl_min = mean(hl_min))
protein_hl_sgd <- protein_hl_sgd01

# mRNA_abun ----
# data from Protein abundances are more conserved than mRNA abundances across diverse taxa
mRNA_abun <- read.csv('./raw_data/mRNA_abun.csv', stringsAsFactors = F)
mRNA_abun_sgd <- mutate(mRNA_abun, mRNA_abun_sum = (mRNA_abun_array+mRNA_abun_seq)/2) %>%
    left_join(SGD, by = c('name' = 'sys_name')) %>% 
    select(c(6,3,4,5))



# ppi ---- 

# data from BIOGRID
ppi <- read.delim("/Users/lipidong/work/protein bundunce/data/PPI.txt", 
    stringsAsFactors = F)
ppi <- ppi[, 6:7]

# TF-target ---- 
#data from YEASTRACT; http:// www.yeastract.com
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


# calculate the indegree
tf_target <- unique(tf_target_sgd[,c(1,3)]) %>% na.omit()
g <- graph.data.frame(tf_target)
is.directed(g)
vcount(g)
in_degree <- degree(g, mode = 'in')
in_degree <- in_degree[unique(tf_target[,2])]
in_degree <- as.data.frame(in_degree)
in_degree$name <- rownames(in_degree)

# dn/ds data ----
# data from Functional genomic analysis of the rates of protein evolution. 
dn_ds <-  read.csv('/Users/lipidong/work/RFile/TF_Target/data/dn_ds.csv', stringsAsFactors = F)
dn_ds_sgd <- 
  left_join(da_ds, SGD, by = c('ORF' = 'sys_name') ) %>% 
  select(c(8,3,4,5,7)) 
colnames(dn_ds_sgd) <- c('sgd_id', 'ds', 'dn', 'dn_ds', 'dn_ds_adj')


# mRNA transcription rate----
# data from A Complete Set of Nascent Transcription Rates for Yeast Genes
trans_rate <- read.csv('./raw_data/transcript_rate.csv', stringsAsFactors = F)




# mapping over
# essetial gene
# system.time({
essential <-   read.delim('./raw_data/gene_essentiality.txt', stringsAsFactors = F) %>% 
filter(sciName == 'Saccharomyces cerevisiae' & essential == 'Y' & pubmedID == 0) %>% 
  left_join(SGD, by=c('locus'='sys_name')) %>% 
  select(sgd_id, locus, essential)
  

#   left_join(SGD, by=c('locus'='sys_name')) %>% 
#   select(sgd_id,essential) %>% 
#   distinct()
# 
# })
# 
# all_data_essential <- all_data %>% 
#    left_join(essential, by = c('sgd_id' = 'sgd_id')) %>% 
#    filter(essential == 'N')
# 
# # calculate the correlation
# all_data <- filter(all_data, is.na(in_degree)==F)
# corr_pre_translation <- corr


# calulate the correlation
all_data <- has_intron
colnum <- ncol(all_data)
corr <- matrix(NA, colnum-2, colnum-2)
colnames(corr) <- colnames(all_data[,3:colnum])
rownames(corr) <- colnames(all_data[,3:colnum])
sapply(1: (colnum-2), function(ii){
  sapply(1: (colnum-2), function(jj){
    print(ii)
    print(jj)
    df <- data.frame(all_data[,ii+2],all_data[,jj+2])
    # df <- data.frame(log(all_data[,ii+2]),log(all_data[,jj+2])) 
    df <- na.omit(df)
    try({
      
        aa <- cor.test(df[,1], df[, 2], method = 'spearman')
        # estimate <- signif(aa$estimate,3)
      # pvalue <- signif(aa$p.value, 3)
      corr[ii,jj] <<- aa$estimate
        # paste(c(estimate, '/',pvalue), collapse = '')
    })
    
    # cor.test(all_data[,ii+2],log2(all_data[,jj+2]), method = 'spearman')$estimate
    
    
  })
})


write.csv(corr, file = './data/yeast_corr_0407.csv')



