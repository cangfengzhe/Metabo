
sgd <- read_delim('/Users/lipidong/work/RFile/TF_Target/raw_data/SGD_features.tab', delim = '\t', col_names = F)

sgd <- filter(sgd, X2 == 'CDS' | X2 == 'ORF') %>% 
  select(c(1,2,4)) %>% 
  distinct()


