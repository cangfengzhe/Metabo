# read the data

# degree----
utr3_degree <- read.csv("/Users/lipidong/baiduyun/work/WX/miRNA_network_analysis/use/gene_indegree_3pUTR.csv", 
  stringsAsFactors = F)

utr5_degree <- read.csv("/Users/lipidong/baiduyun/work/WX/miRNA_network_analysis/use/gene_indegree_5pUTR.csv", 
  stringsAsFactors = F)

cds_degree <- read.csv("/Users/lipidong/baiduyun/work/WX/miRNA_network_analysis/use/gene_indegree_CDS.csv", 
  stringsAsFactors = F)

# 
# gene_degree <- read.csv("/Users/lipidong/baiduyun/work/WX/miRNA_network_analysis/use/gene_indegree_Intergenic_ncRNA.csv", 
#     stringsAsFactors = F)
# 
# gene_degree <- read.csv("/Users/lipidong/baiduyun/work/WX/miRNA_network_analysis/use/gene_indegree_Overlapping_ncRNA.csv", 
#                         stringsAsFactors = F)

convert <- function(gene_degree){
  gene_degree$ensembl <- do.call("rbind", strsplit(gene_degree[, 1], split = "\\."))[, 1]
  gene_degree <- gene_degree[, c(3, 2)]
  
  gene_degree
}

utr3_degree <- convert(utr3_degree)
utr5_degree <- convert(utr5_degree)
cds_degree <- convert(cds_degree)
colnames(utr3_degree) <- c('ensg', 'utr3_degree')
colnames(utr5_degree) <- c('ensg', 'utr5_degree')
colnames(cds_degree) <- c('ensg', 'cds_degree')


# mircode <- read.delim("/Users/lipidong/work/WX/mircode_highconsfamilies.txt", 
#   stringsAsFactors = F)
# ensembl <- strsplit(mircode[, 1], "\\.")
# mircode$ensembl <- do.call("rbind", ensembl)[, 1]
# mircode <- mircode[, c(13, 4)]

ensembl2genbank <- read.csv("/Users/lipidong/baiduyun/work/WX/ensemble2genbank.txt", 
  stringsAsFactors = F)
colnames(ensembl2genbank) <- c("ensembl_id", "entrez_id", "genbank_acc")
ensembl2genbank <- read.csv("/Users/lipidong/baiduyun/work/WX/ensembl_protein_gene.csv", 
  stringsAsFactors = F)
colnames(ensembl2genbank) <- c("entrez", "ensembl_pro", "ensembl_gene", 
  "genbank")

# 这里的mRNA是decay rate
mRNA_hl <- read.csv("/Users/lipidong/baiduyun/work/WX/human mRNA half life data.csv", 
  stringsAsFactors = F)[, c(1, 3, 4)]

mRNA_hl1 <- sqldf("select * from mRNA_hl left join ensembl2genbank on mRNA_hl.Accession = ensembl2genbank.genbank")[,c(5,2)]


mRNA_decayrate <- mRNA_hl1 %>% distinct() %>% na.omit() %>% 
  rename(ensp = ensembl_pro, mRNA_decay_rate = Rate)


# pro_hl----
protein_id <- read_csv('./raw_data/protein_ids.txt', col_names = c('enst', 'uniprot', 'ottp', 'ref_np', 'ensp'), skip = 1)


pro_hl_raw <- read.csv("/Users/lipidong/baiduyun/work/WX/protein_hl.csv", stringsAsFactors = F)
pro_hl_raw <- pro_hl_raw[, c(1, 4, 5)]
colnames(pro_hl_raw) <- c("ipi", "pro_turnover", "pro_whole_half_life")

pro_hl1 <- left_join(pro_hl_raw, ipi2ensembl, by = c('ipi' = 'ipi')) %>% 
  select(ensembl_pro, pro_turnover) %>% 
  na.omit() %>% 
pro_hl_1 <- inner_join(pro_hl1, protein_id, by = c('ensembl_pro' = 'ensp')) %>% 
  select(enst, pro_turnover)
pro_hl_2 <- inner_join(pro_hl1, protein_id, by = c('ensembl_pro' = 'uniprot')) %>% 
  select(enst, pro_turnover)

pro_hl_3 <- inner_join(pro_hl1, protein_id, by = c('ensembl_pro' = 'ottp')) %>% 
  select(enst, pro_turnover)

pro_hl_4 <- inner_join(pro_hl1, protein_id, by = c('ensembl_pro' = 'ref_np')) %>% 
  select(enst, pro_turnover)
pro_turnover <- rbind_all(list(pro_hl_1, pro_hl_2, pro_hl_3, pro_hl_4)) %>% distinct()

# protein abundance
pro_abun <- read.table("~/baiduyun/work/WX/protein_abundance.txt", stringsAsFactors = F, 
  header = T)
colnames(pro_abun) <- c("id", "ensembl_pro", "pro_abundance")
tmp <- do.call("rbind", strsplit(pro_abun[, 2], "\\."))[, 2]
pro_abun[, 2] <- tmp
pro_abun <- pro_abun[, 2:3]



# NR_hl <- read.csv("/Users/lipidong/work/WX/NR_hl.csv", stringsAsFactors = F)
# NR_hl <- left_join(NR_hl, ref2entrez, by = c(name = "accession"))
# NR_hl_tmp <- sqldf("select  gene_id,avg(RPKM) as RPKM, avg(hl) as hl from NR_hl group by gene_id")
# NR_hl_tmp[NR_hl_tmp$hl == 0, 3] <- NA
# NR_hl <- NR_hl_tmp
# 
# genbank_hl <- read.csv("/Users/lipidong/work/WX/genbank_hl.csv", 
#   stringsAsFactors = F)
# genbank_hl <- left_join(genbank_hl, ensembl2genbank, by = c(name = "genbank"))
# genbank_hl <- genbank_hl[, c(7, 1, 3, 4)]
# genbank_hl_tmp <- sqldf("select ensembl_gene, avg(RPKM) as RPKM, avg(hl) as hl from genbank_hl group by ensembl_gene")
# genbank_hl_tmp[genbank_hl_tmp[, 3] == 0, 3] <- NA
# genbank_hl <- genbank_hl_tmp

# mRNA abundance ----
mRNA_abun <- read.csv("/Users/lipidong/baiduyun/work/WX/rna_abundance.csv", 
  stringsAsFactors = F)
tissue <- data.frame(tissue = unique(mRNA_abun[, 2])[45:76])
mRNA_abun_tissue <- sqldf("select mRNA_abun.* from mRNA_abun inner join tissue on tissue.tissue=mRNA_abun.Sample")
# mRNA_abun_tissue <- sqldf("select distinct Gene,avg(value) as avg from mRNA_abun_tissue group by Gene")
mRNA_abun_tissue <- group_by(mRNA_abun_tissue, Gene) %>% 
  summarise(mRNA_tissue_abun_avg = mean(Value), mRNA_tissue_abun_std = sd(Value))

# PARS score ----
# method using MBE
PARS01 <- read.csv("/Users/lipidong/work/WX/pars_GM12892_MBE.csv", 
  stringsAsFactors = F)
PARS02 <- read.csv("/Users/lipidong/work/WX/pars_GM12891_MBE.csv", 
  stringsAsFactors = F)
PARS03 <- read.csv("/Users/lipidong/work/WX/pars_GM12878_MBE.csv", 
  stringsAsFactors = F)
PARS01 <- PARS01[, c(2, 5, 6)]
PARS02 <- PARS02[, c(2, 5, 6)]
PARS03 <- PARS03[, c(2, 5, 6)]
colnames(PARS01) <- c("name", "all_score", "part_score")
colnames(PARS02) <- c("name", "all_score", "part_score")
colnames(PARS03) <- c("name", "all_score", "part_score")

PARS <- sqldf("select PARS01.*, PARS02.all_score, PARS02.part_score, PARS03.all_score, PARS03.part_score from PARS01 left join PARS02 on PARS01.name=PARS02.name left join PARS03 on PARS03.name=PARS01.name")

PARS$all_score_mean <- rowMeans(PARS[, c(2, 4, 6)])
PARS$part_score_mean <- rowMeans(PARS[, c(3, 5, 7)])
PARS <- PARS[, c(1, 8, 9)]
PARS_id <- sqldf("select ref2entrez.gene_id,PARS.name, PARS.all_score_mean, PARS.part_score_mean from PARS left join ref2entrez on PARS.name = ref2entrez.accession")
PARS$name2 <- do.call("rbind", strsplit(PARS[, 1], split = "\\."))[, 
  1]

PARS1 <- inner_join(PARS, ensg2refseq, by = c('name2' = 'ref_mRNA')) %>%
  select(ensg, enst, all_score_mean)

PARS2 <- inner_join(PARS, ensg2refseq, by = c('name2'= 'ref_ncRNA')) %>% 
  select(ensg, enst, all_score_mean)

PARS3 <- PARS %>%  inner_join(ensg2refseq, by = c('name2' = 'enst')) %>% 
  rename(enst = name2) %>% 
  select(ensg, enst, all_score_mean)

pars_mbe <- rbind_all(list(PARS1, PARS2, PARS3))

pars <- left_join(pars_nat, pars_mbe, by = 'enst') %>% 
  rename(ensg = ensg.x, pars_mbe = all_score_mean) %>%   select(ensg, enst, pars_nat, pars_mbe) 

  

View(pars)

library(sqldf)
library(RSQLite)

enst_len_raw <- read.csv('/Users/lipidong/work/WX/ENST_length.csv', stringsAsFactors = F)
colnames(enst_len_raw) <- c('ensg', 'enst', 'length')
enst_len <- group_by(enst_len_raw, ensg) %>% 
  summarise(mRNA_max_length = max(length))

# cg content ----
# data from ensembl
cg_raw <- read_csv('./raw_data/cg_content.txt') %>% na.omit()
colnames(cg_raw) <- c('ensg', 'enst', 'cg')
cg <- cg_raw %>%  group_by(ensg) %>% 
  summarise( cg = mean(cg, na.rm = T))

# dn_ds ----

dn_ds_raw <- read_csv('./raw_data/dn_ds.txt')
colnames(dn_ds_raw) <- c('ensg', 'dn1','ds1', 'dn2', 'ds2', 'dn3', 'ds3')

dn_ds_1 <-   dn_ds_raw %>% mutate(dn_ds1 = (dn1/ds1), dn_ds2 = dn2/ds2, dn_ds3 = dn3/ds3)

dn_ds_1[which(is.infinite(dn_ds_1$dn_ds1) == T), 8] <- NA
dn_ds_1[which(is.infinite(dn_ds_1$dn_ds2) == T), 9] <- NA
dn_ds_1[which(is.infinite(dn_ds_1$dn_ds3) == T), 10] <- NA

dn_ds_1$dn_ds <- sapply(1: nrow(dn_ds_1), function(ii){
  mean(c(dn_ds_1$dn_ds1[ii], dn_ds_1$dn_ds2[ii], dn_ds_1$dn_ds3[ii]), na.rm = T)
})

dn_ds <- dn_ds_1 %>% select(ensg, dn_ds) %>% 
  group_by(ensg) %>% 
  summarise(dn_ds = mean(dn_ds, na.rm = T))
# essential ----
essen_gene <- read_tsv('./raw_data/gene_essentiality.txt') %>% 
  filter(grepl('Homo sapiens',sciName) == T) %>% 
  select(locus, essential) %>% 
  mutate(bool = ifelse(essential=='Y', 1, 0))  %>% 
  group_by(locus) %>% 
  summarise(mean = mean(bool)) %>% 
  filter(mean >0.5) %>% 
  rename(ensg = locus, essen = mean) %>% 
  mutate(essen = 1)


# main ----

enst_mapping <- ensg_enst_ensg %>% 
  left_join(utr5_degree, by = 'ensg') %>% 
  left_join(utr3_degree, by = 'ensg') %>% 
  left_join(cds_degree, by = 'ensg') %>% 
  left_join(pars, by = 'enst') %>%
  rename(ensg = ensg.x) %>% 
  left_join(mRNA_abun_tissue, by = c('ensg' = 'Gene')) %>% 
  left_join(mRNA_decayrate, by = 'ensp') %>% 
  left_join(pro_abun, by = c('ensp' = 'ensembl_pro')) %>% 
  left_join(pro_turnover, by = 'enst') %>% 
  left_join(cg, by = 'ensg') %>% 
  left_join(dn_ds, by = 'ensg') %>% 
  left_join(intron_count, by = c('enst')) %>% 
  distinct() %>% 
  select(-matches('ensg.y')) %>% 
  as.data.frame()
View(enst_mapping)
l_ply(4:ncol(enst_mapping), function(ii){
  print(ii)
  enst_mapping[, ii] <<- as.numeric(enst_mapping[, ii]) 
})

cor(enst_mapping[,-c(1:3)], use = 'na.or.complete', method = 'spearman') %>% View()


## focus on the gene
ensg_mapping <- enst_mapping %>% group_by(ensg) %>% 
  summarise(utr5_degree = mean(utr5_degree, na.rm = T),
            utr3_degree = mean(utr3_degree, na.rm = T),
            cds_degree = mean(cds_degree, na.rm = T),
            pars_nat = mean(pars_nat, na.rm = T),
            pars_mbe = mean(pars_mbe, na.rm = T),
            mRNA_tissue_abun_avg = mean(mRNA_tissue_abun_avg, na.rm = T),
            mRNA_decay_rate = mean(mRNA_decay_rate, na.rm = T),
            pro_abundance = mean(pro_abundance, na.rm = T),
            pro_turnover = mean(pro_turnover, na.rm = T),
            cg = mean(cg, na.rm = T),
            dn_ds = mean(dn_ds, na.rm = T),
            intron_count = sum(intron_count, na.rm = T)) 

View(ensg_mapping)
cor_ensg <- cor(ensg_mapping[,-c(1)], use = 'na.or.complete', method = 'spearman') 

# essential gene ----
ensg_essen <- essen_gene %>% left_join(ensg_mapping, by = c('ensg' = 'ensg'))
cor_ensg_essen <- cor(ensg_essen[,-c(1)], use = 'na.or.complete', method = 'spearman') 

# house keeping gene -----
house_keep <- read_csv('./raw_data/house_keeping.csv') %>% 
  left_join(ensg_enst_entrez, by = c('ENTREZ_GENE_ID' = 'entrez')) %>% 
  select(ENTREZ_GENE_ID, ensg) %>% 
  rename(entrez = ENTREZ_GENE_ID) %>% 
  distinct() %>% 
  filter( grepl('ENSG', ensg) == T) %>% 
  select(ensg) %>% 
  mutate(house_keep = 1)


# calculate the difference between essential gene and unessential
ensg_mapping <- ensg_mapping %>% left_join(essen_gene, by = 'ensg') %>% left_join(house_keep, by = 'ensg')

View(ensg_mapping)
ensg_mapping[which(is.na(ensg_mapping$essen == T)),
'essen'] <- 0

ensg_mapping[which(is.na(ensg_mapping$house_keep == T)), 'house_keep'] <- 0

ensg_mapping <- as.data.frame(ensg_mapping)

ensg_mapping$essen <- ensg_mapping$essen %>% as.factor() 
ensg_mapping$house_keep <- ensg_mapping$house_keep %>% as.factor() 



ensg_diff <- ldply( 2: (length(ensg_mapping) - 2), function(ii){
  
  
  essen_pval <- wilcox.test(ensg_mapping[, ii] ~ ensg_mapping$essen)$p.value
  essen_count <- ensg_mapping %>% filter(essen == 1) %>% .[, ii] %>% na.omit() %>% length()
  unessen_count <- ensg_mapping %>% filter(essen == 0) %>% .[, ii] %>% na.omit() %>% length()
  
  essen_median <- median(ensg_mapping %>% filter(essen == 1) %>% .[, ii], na.rm = T)
  unessen_median <- median(ensg_mapping %>% filter(essen == 0) %>% .[, ii], na.rm = T)
  
  house_keep_pval <- wilcox.test(ensg_mapping[, ii]~ ensg_mapping$house_keep) $p.value
  
  house_keep_count <- ensg_mapping %>% filter(house_keep == 1) %>% .[, ii] %>% na.omit() %>% length()
  unhouse_keep_count <- ensg_mapping %>% filter(house_keep == 0) %>% .[, ii] %>% na.omit() %>% length()
  
  house_keep_median <- median(ensg_mapping %>% filter(house_keep == 1) %>% .[, ii], na.rm = T)
  unhouse_keep_median <- median(ensg_mapping %>% filter(house_keep == 0) %>% .[, ii], na.rm = T)
  
  c(colnames(ensg_mapping)[ii], essen_pval,essen_count, unessen_count, essen_median, unessen_median, house_keep_pval, house_keep_count, unhouse_keep_count, house_keep_median, unhouse_keep_median)
})

colnames(ensg_diff) <- c('var_name', 'essential_pvalue','essential_count', 'unessential_count', 'essential_median', 'unessential_median', 'house_keep_pvalue', 'house_keep_count', 'unhouse_keep_count', 'house_keep_median', 'unhouse_keep_median')

write.csv(ensg_diff, file = './data/ensg_diff_essential_house_keep0428.csv')






cor_ensg_house <- cor(house_keep_ensg[,-c(1)], use = 'na.or.complete', method = 'spearman') 


# degree bin
library(ggplot2)
ggplot(ensg_mapping, aes(x = utr5_degree)) +
  geom_bar(na.rm = T)
  
geom_density(na.rm = T)
  
filter(ensg_mapping, utr5_degree ==0) %>% nrow()

table(ensg_mapping[,c(2)]) %>% View


View(cor_ensg_house)
cor_ensg_house %>% View
View(cor_ensg_house)
diff <- (cor_ensg_house-cor_ensg) 
library(reshape2)
cor_1 <- melt(cor_ensg)
cor_2 <- melt(cor_ensg_house)
cor_3 <- melt(diff)

cor_bind <- left_join(cor_1, cor_2, by = c('Var1', 'Var2')) %>% rename(ensg = value.x, house = value.y) %>% 
  mutate(diff = ensg - house)

cor_bind$Var1 <- factor(cor_bind$Var1, levels = unique(cor_bind$Var1))
cor_bind$Var2 <- factor(cor_bind$Var2, levels = unique(cor_bind$Var1))
cor_bind$name_diff <- as.numeric(cor_bind$Var1) - as.numeric(cor_bind$Var2)

cor_bind$Var1 <- as.character(cor_bind$Var1)
cor_bind$Var2 <- as.character(cor_bind$Var2)

cor_bind <- cor_bind %>% 
  filter(name_diff>0 & (diff > 0.1 | diff< (-0.1)))


cor_bind$txt_bind <- paste(cor_bind$Var1, cor_bind$Var2, sep = '.')

library(ggplot2)
g <- ggplot(cor_bind, aes(x=txt_bind, y = ensg)) +
  geom_bar(stat = 'identity')
g

View(cor_bind)


save(cor_ensg_house, cor_ensg, diff, file = './data/xinyu.rdata')
View(cor_ensg)



save.image('./data/enst.rdata')

ensg_essen




