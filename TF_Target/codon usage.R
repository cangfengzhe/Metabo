library(seqinr)
library(splitstackshape)
cds <- read.fasta(file = './raw_data/cds.fasta', as.string = T)
cds_df <- do.call('rbind', cds) %>% as.data.frame()
cds_df$name <- rownames(cds_df)
cds_df <- cds_df %>% filter(V1 != 'sequence unavailable')
cds_df <- cSplit(cds_df, 2, sep = '|', direction = 'wide')
cds_df <- cds_df %>% as.data.frame()
# 必须转成 list， 很无语
cds_df[,1] <- as.character(cds_df[, 1])
aa <- cds_df[,1] %>% as.character()
aa <- as.list(aa)

write.fasta(aa, cds_df[,2], file.out = './data/cds_filter.fasta')



# 转由codonw计算 CAI CBI----

cai_cbi <- read_delim('./raw_data/cds_cai_cbi.out', delim = ' ') %>% select(-4) %>% 
  dplyr::rename(name = title)
# calculate the codon usage indice
condon_colnames <- uco(seq = s2c(cds_df[ii,1]), as.data.frame = T) %>% rownames()

condon_usage <- ldply(1: nrow(cds_df), .progress = "text" , function(ii){
  uco(seq = s2c(cds_df[ii,1]), as.data.frame = T)[,4] %>% t()
  
})
colnames(condon_usage) <- condon_colnames
condon_usage$name <- cds_df[,2] %>% as.character()
condon_usage <- condon_usage[,c(65, 1:64)]
condon_usage <- condon_usage %>% left_join(cai_cbi, by = 'name')
condon_usage <- condon_usage %>% select(c(1,66,67,2:65))
condon_usage <- condon_usage %>% select(c(3, 1, 2, 4:67))
l_ply(4:67, function(ii){
  condon_usage[, ii] <<- round(condon_usage[, ii], 3)
})
View(condon_usage)

write_csv(condon_usage, path = './data/condon_usage.csv')


yeast_data <- all_data %>% left_join(condon_usage, by = c('sys_name' = 'name'))

yeast_mRNA <- yeast_data[, c(7, 8, 9, 10, 22)]
yeast_codon <- yeast_data[, c(32:97)]

yeast_cor <- matrix(NA, 66, 5)
colnames(yeast_cor) <- yeast_mRNA %>% colnames()

rownames(yeast_cor) <- yeast_codon %>% colnames()
View(yeast_cor)

l_ply(1:66, .progress = 'text', function(ii){
  l_ply(1:5, function(jj){
    
    df <- data.frame(yeast_codon[,ii], yeast_mRNA[,jj])
    df <- na.omit(df)
    
    estim <- cor(df[,1], df[, 2], method = 'spearman')
    estim <- round(estim, 3)
    yeast_cor[ii,jj]  <<- estim
  })
})

write.csv(yeast_cor, file = './data/cor_codon.csv')


half_life2015 <- read_csv('./raw_data/half_life_2015.csv')

yeast_data1 <- left_join(yeast_data,half_life2015, by = c('sys_name' = 'name'))

cor(yeast_data1$mRNA_half_life2015, yeast_data1$CAI, use = 'na.or.complete', method = 'p')

write_csv(yeast_data1, './data/yeast_data_halflife_2015.csv')


#2015 half_life correlation

yeast_cor_hl_2015 <- matrix(NA,98,3)
ldply(3:98, function(ii){
  print(ii)
 cor_value1 <-  cor(yeast_data1$mRNA_half_life2015, yeast_data1[,ii], use = 'na.or.complete', method = 'spearman')
 cor_value2 <-  cor(yeast_data1$mRNA_halflife, yeast_data1[,ii], use = 'na.or.complete', method = 'spearman')
 yeast_cor_hl_2015[ii,1] <<- colnames(yeast_data1)[ii]
 yeast_cor_hl_2015[ii,2] <<- cor_value1
 yeast_cor_hl_2015[ii,3] <<- cor_value2
 
})

colnames(yeast_cor_hl_2015) <- c('cor', 'half_life_2015', 'half_life_2014')
yeast_cor_hl_2015 <- yeast_cor_hl_2015 %>% na.omit()
write_csv(yeast_cor_hl_2015 %>% as.data.frame(), './data/cor_halflife_2015.csv')
yeast_cor_hl_2015 %>% class
View(yeast_cor_hl_2015)
View(cor)
yeast_cor %>% View

aa <- cor(yeast_data[,3:34],use = 'pairwise.complete.obs', method = 'spearman' )

View(aa)
View(yeast_data)
colnames(yeast_data)


