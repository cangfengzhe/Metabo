mapping <- read_csv('./raw_data/yeast_idmapping.txt') %>% na.omit()

housekeep <- read_delim('./raw_data/yeast01.txt', delim='\t')[,1] %>% 
  rbind(read_delim('./raw_data/yeast02.txt', delim='\t')[,1]) %>% 
 rbind(read_delim('./raw_data/yeast03.txt', delim='\t')[,1]) %>% 
  rbind(read_delim('./raw_data/yeast04.txt', delim='\t')[,1]) %>% 
  rbind(read_delim('./raw_data/yeast05.txt', delim='\t')[,1]) %>% 
  unique()  
colnames(housekeep)[1] <- 'gene'
colnames(mapping) <- c('gene', 'sgd_id')
house_keep <- housekeep %>% left_join(mapping, by = 'gene') %>% na.omit()
colnames(house_keep)
house_keep_pars <- house_keep %>% inner_join(all_data, by = 'sgd_id') %>% 
  select(pars) %>% 
  na.omit()

go_stress <- read_csv('./raw_data/yeast_go_stress.csv', col_names = F) 
go_stress_pars <- go_stress %>% left_join(all_data, by = c('X1' = 'sgd_id')) %>% 
  select(pars) %>% na.omit()

nrow(go_stress)
nrow(house_keep)
intersect(house_keep$sgd_id, go_stress$X1) %>% length

# essential ------
essential <- read.delim('./raw_data/gene_essentiality.txt', stringsAsFactors = F) %>% 
  filter(sciName == 'Saccharomyces cerevisiae' & essential == 'Y' & pubmedID == 0) %>% 
  left_join(SGD, by=c('locus'='sys_name')) %>% 
  select(sgd_id, essential) %>% unique() 

essen <- filter(essential, essential == 'Y') %>% unique()
nrow(essen)

chongfu <- intersect(essential$sgd_id, go_stress$X1)
length(chongfu)

essen_pars <- essen %>% left_join(all_data, by = 'sgd_id') %>% 
  select(pars) %>% na.omit() 
shapiro.test(essen_pars[,1])

go_stress_pars <- as.data.frame(go_stress_pars)
wilcox.test(essen_pars[,1] %>% as.numeric(), go_stress_pars[,1] %>% as.numeric())


# 去重 ----
essen_qc <- setdiff(essen$sgd_id, chongfu) %>% as.data.frame()
stress_qc <- setdiff(go_stress$X1, chongfu) %>% as.data.frame()
colnames(stress_qc) <- 'X1'

go_stress_qc_pars <- stress_qc %>% left_join(all_data, by = c('X1' = 'sgd_id')) %>% 
  select(pars) %>% na.omit()
colnames(essen_qc) <- 'sgd_id'
essen_qc_pars <- essen_qc %>% left_join(all_data, by = 'sgd_id') %>% 
  select(pars) %>% na.omit() 

wilcox.test(essen_qc_pars[,1] %>% as.numeric(), go_stress_qc_pars[,1] %>% as.numeric())


essen_qc_pars[,1] %>% length()
go_stress_qc_pars %>% nrow


# GO enrichment -----
threhold <- mean(all_data$pars  %>% na.omit())
big_pars <- filter(all_data, pars>threhold) %>% select(sgd_id)

small_pars <- filter(all_data, pars<=threhold) %>% select(sgd_id)
View(big_pars)
View(small_pars)
write_csv(big_pars, './data/big_pars.csv')
write_csv(small_pars, './data/small_pars.csv')
library(topGO)
browseVignettes("topGO")

# go cc -----
go_cc <- read_csv('./raw_data/yeast_go_cc.csv', col_names = F)

dplyr::intersect(go_stress[,1] %>% as.character(), go_cc[,1] %>% as.character()) 

no_go_stress <- setdiff(all_data$sgd_id, go_stress$X1)
no_go_stress <- no_go_stress %>%
  as.data.frame 
colnames(no_go_stress) <- 'sgd_id'
no_go_stress <- no_go_stress %>% 
  left_join(all_data, by = 'sgd_id') %>% 
  select(pars) %>% as.data.frame()

go_stress_pars <- as.data.frame(go_stress_pars)
wilcox.test(go_stress_pars[,1] %>% as.numeric(), no_go_stress[,1] %>% as.numeric())


class(go_stress_pars)
