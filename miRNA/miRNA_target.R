# 高度保守
mir_target_h <- read_tsv('./raw_data/mircode_highconsfamilies.txt') %>% select(1, 4, 12) 
mir_target_m <- read_tsv('./raw_data/mircode_medconsfamilies.txt') %>% select(1, 4, 12) 
mir_target <- bind_rows(list(mir_target_h, mir_target_m)) %>% distinct()

library(splitstackshape)
mir_target <- as.data.frame(mir_target)
mir_tar <- cSplit(mir_target, 3, sep = ',', direction = 'long') %>% unique()


# 单个位点，两个位点， 三个位点
unique(mir_target[,3])
mir_target[, 3] <- gsub('ncRNA,', '', mir_target[, 3])

utr3 <- filter(mir_target, tr_region == '3pUTR,') %>% 
  group_by(gene_id) %>% 
  summarise( num = n())

utr5 <- filter(mir_target, tr_region == '5pUTR,') %>% 
  group_by(gene_id) %>% 
  summarise( num = n())
cds <- filter(mir_target, tr_region == 'CDS,') %>% 
  group_by(gene_id) %>% 
  summarise( num = n())

utr35 <- filter(mir_target, tr_region == '3pUTR,5pUTR,') %>% 
  group_by(gene_id) %>% 
  summarise( num = n())

utr3cds <- filter(mir_target, tr_region == '3pUTR,CDS,') %>% 
  group_by(gene_id) %>% 
  summarise( num = n())

utr5cds <- filter(mir_target, tr_region == 'CDS,5pUTR,') %>% 
  group_by(gene_id) %>% 
  summarise( num = n())

utr35cds <- filter(mir_target, tr_region == '3pUTR,CDS,5pUTR,')%>% 
  group_by(gene_id) %>% 
  summarise( num = n())

all_mir <- group_by(mir_target, gene_id) %>% 
  summarise(num = n())

mir2map <- function(df, colname){
  df <- as.data.frame(df)
  df_tmp <- strsplit(df[, 1], split = '\\.')
  df[,1] <- do.call('rbind', df_tmp)[, 1]
  
  ensg_mapping2 <<- left_join(ensg_mapping2, df, by = c('ensg' = 'gene_id'))
 # colnames(ensg_mapping2)[ncol(ensg_mapping2)] <<- colname
  cat('finish')
}

mir2map(utr3)
mir2map(utr5)

mir2map(cds)
mir2map(utr35)
mir2map(utr3cds)
mir2map(utr5cds)
mir2map(utr35cds)
mir2map(all_mir)
colnames(ensg_mapping2)[16:23] <- c('utr3', 'utr5', 'cds', 'utr35', 'utr3cds', 'utr5cds', 'utr35cds', 'all_mir')

