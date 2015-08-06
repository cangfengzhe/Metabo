gene_dis <- read_tsv('./raw_data/curated_gene_disease.txt')
colnames(gene_dis)
unique(gene_dis$diseaseName) %>% length
nrow(gene_dis)

# icd ----
icd_gene <- read.csv('./raw_data/icd_gene.csv', stringsAsFactors = F)
gene_list <- icd_gene[, 3:ncol(icd_gene)]
col_len <-  ncol(gene_list)

gene_list1 <- ldply(1:nrow(icd_gene), function(ii){
  which(gene_list[ii, 1: col_len] != gene_list[1, 10]) %>% gene_list[ii,.] %>% paste(collapse=',')
})

gene_list1$dis <- icd_gene[,1]

library(splitstackshape)
gene_list2 <- cSplit(gene_list1, 1, sep = ',', direction = 'long')
gene_list2 <- as.data.frame(gene_list2)
icd_class <- read.csv('./raw_data/icd_class', header = F, stringsAsFactors = F)
gene_list2$class <- NA
gene_list2$dis <- as.integer(gene_list2$dis)
sapply(1:nrow(icd_class), function(ii){
  index <- which(gene_list2[, 2]>=icd_class[ii, 1] & gene_list2[,2]<=icd_class[ii,2])
  gene_list2$class[index] <<- icd_class[ii,3]
  
})

library(org.Hs.eg.db)
symbl <- org.Hs.egSYMBOL %>% as.data.frame()
View(symbl)
gene_list2$V1 <- as.character(gene_list2$V1)
gene_list4 <- left_join(gene_list2, symbl, by = c('V1'='symbol')) %>% distinct()


# aa <- function(x) paste(x, collapse = ',')
save.image()

gene_list5 <- unique(gene_list4[, c(3, 4)]) %>% na.omit() 
gene_list5$gene_id <- gene_list5$gene_id %>% as.integer()
gene_list_ensg <- gene_list5 %>% left_join(ensg_enst_entrez, by = c('gene_id' = 'entrez')) %>% dplyr::select(class, ensg) %>% distinct()

ensg_mapping_icd <- ensg_mapping %>% 
  left_join(gene_list_ensg, by = 'ensg') %>% 
  filter(is.na(class) == F)


enst_icd_class <- ensg_mapping_icd %>% 
  group_by(class) %>% 
  summarise(mean(pars_nat, na.rm = T), n())

View(enst_icd_class)
enst_icd_class
