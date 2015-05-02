# ensg_bin 
# load('./data/enst.rdata')
# bin 1-5, 6-10, 11-20, 21-30, 30-50, 50以上
table(ensg_mapping$utr3_degree) %>% View
library(reshape2)
# utr5_degree
cor(a[,3], a[,4])
class(a[,3])
ldply(2:13, class)
library(reshape2)


cor0 <- ensg_mapping %>% 
  .[, -c(1, 14, 15)] %>% 
  cor(., use = 'na.or.complete', method = 'spearman') %>% 
  melt() %>% 
  filter(value>0.1 & value !=1)

cor_bin <- function(col, min, max, name){
  # col position of column
  tmp <- paste0('cor',name)
  cor1 <- filter(ensg_mapping, ensg_mapping[,col] > min & ensg_mapping[,col]<= max) %>% 
    .[, -c(1:4, 14, 15)] %>% 
    cor(., use = 'na.or.complete', method = 'spearman') %>%
    melt() %>%  
    filter(value>0.1 & value !=1) 
    colnames(cor1) <- c('name1', 'name2', tmp)
    cor1
}




l_ply(1: 6, function(ii){
  print(ii)
  sifenshu <- quantile(ensg_mapping$utr5_degree, probs = seq(0, 1, 0.2) , na.rm = T)
  switch(ii,{min = 0; max = 10000}, 
    {min = 0; max = sifenshu[2]}, 
    {min = sifenshu[2]; max = sifenshu[3]},
    {min = sifenshu[3]; max = sifenshu[4]},
    {min = sifenshu[4]; max = sifenshu[5]},
    {min = sifenshu[5]; max = sifenshu[6]})
  tmp <- paste0('cor', ii-1)
  assign(tmp, value = cor_bin(4,min, max, ii-1 ), envir = .GlobalEnv) 
})

name <- unique(cor0[,1])

cor_cds <- cor0 %>%
  left_join(cor1, by = c('name1' = 'name1', 'name2' = 'name2'))%>% 
  left_join(cor2, by = c('name1' = 'name1', 'name2' = 'name2'))%>%
  left_join(cor3, by = c('name1' = 'name1', 'name2' = 'name2'))%>% 
  left_join(cor4, by = c('name1' = 'name1', 'name2' = 'name2'))%>% 
  left_join(cor5, by = c('name1' = 'name1', 'name2' = 'name2')) %>% 
  mutate(diff = factor(name1 , levels= name) %>% as.integer() - factor(name2, levels=name) %>% as.integer()) %>% 
  filter(diff < 0) %>% 
  select(-c(diff))
View(cor_cds)

quote()

