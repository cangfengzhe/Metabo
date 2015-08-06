library(splitstackshape)
gpl <- read_delim('/Users/lipidong/Downloads/GPL13667-15572.txt', delim = '\t', skip = 43) %>% select(c(1, 15, 21)) %>% as.data.frame()
gpl1 <- cSplit(gpl, 2, sep = '///', direction = 'wide') %>% select(1:3) %>% 
  cSplit(2, sep = '///', direction = 'wide') %>% 
  select(1:3) %>% 
  as.data.frame()
colnames(gpl1) <- c('probe_id', 'gene_symbol', 'gene_id')
rm(gpl)
gc()
