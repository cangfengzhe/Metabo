# 整理enst的pars和decay rate



ensg_enst_ensg <- read_csv('./raw_data/ensg_enst_ensp_entrez.txt')[,c(1,2,5)] 
colnames(ensg_enst_ensg) <- c('ensg', 'enst', 'ensp')

#calulate the intron 
transcript_raw <- read_csv('./raw_data/human_extron_transcript_length.txt', col_names = c('ensg', 'enst', 'exon_s', 'enon_e', 'trans_len'), skip = 1)

intron_count <- transcript_raw %>% group_by(enst) %>% 
  summarise(intron_count = n()-1)

enst_pars_decay <- ensg_enst_ensg %>% left_join(pars, by = 'enst') %>% 
  left_join(mRNA_decayrate, by = 'ensp') %>% 
  select(c(3, 2, 5:7)) %>% 
  left_join(intron_count, by = c('enst')) %>% 
  distinct() %>% 
  filter(!(is.na(pars_nat) ==T & is.na(pars_mbe) == T & is.na(mRNA_decay_rate) == T))


colnames(enst_pars_decay)[2] <- 'ensg'
write_csv(enst_pars_decay, './data/enst_pars_mRNA_decay.csv')
corr_hasintron <- cor(enst_pars_decay %>% filter(intron_count>0) %>% select(c(3:5)), use = 'na.or.complete', method = 'spearman' )

corr_all <- cor(enst_pars_decay  %>% select(c(3:5)), use = 'na.or.complete', method = 'spearman' )

write.csv(corr_all %>% as.data.frame(),'./data/correlation.csv')

write.csv(corr_hasintron %>% as.data.frame(),'./data/correlation.csv')
