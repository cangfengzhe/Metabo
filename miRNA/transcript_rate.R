transcript_rate <- read_csv('./raw_data/transcript_rate.csv')

ensg_enst_entrez$entrez <- as.character(ensg_enst_entrez$entrez)

transcript_rate2 <- transcript_rate %>% left_join(gene2ref, by = c('transcript' = 'accession')) %>% 
  left_join(ensg_enst_entrez, by = c('gene_id' = 'entrez')) %>%
  select(ensg,Velocity) %>% distinct()

# transcript rate
ensg_mapping_tr <- ensg_mapping3 %>% left_join(transcript_rate2, by = 'ensg')
cor <- corr_f(ensg_mapping_tr, c(1, 14, 15))
View(cor)


# transcript initial rate
transcript_init_rate <- read_csv('./raw_data/transcript_init_rate.csv')

ensg_enst_entrez$entrez <- as.character(ensg_enst_entrez$entrez)

transcript_init_rate <- transcript_init_rate %>% left_join(gene2ref, by = c('transcript' = 'accession')) %>% 
  left_join(ensg_enst_entrez, by = c('gene_id' = 'entrez')) %>%
  select(ensg,init_rate) %>% distinct()

# transcript rate
ensg_mapping_tr <- ensg_mapping_tr %>% left_join(transcript_init_rate, by = 'ensg')
cor <- corr_f(ensg_mapping_tr, c(1, 14, 15))
View(cor)
