library(seqinr)
library(splitstackshape)
utr3_seq <- read.fasta('./raw_data/human_utr3.fasta', as.string = T)
utr5_seq <- read.fasta('./raw_data/human_utr5.fasta' , as.string = T)
cds_seq <- read.fasta('./raw_data/human_cds.fasta', as.string = T)

seq_proc <- function (seq) {
  utr3_df <- do.call('rbind', seq) %>% as.data.frame()
  utr3_df$name <- rownames(utr3_df)
  utr3_df[,1] <- as.character(utr3_df[,1])
  utr3_df$len <- sapply(1:nrow(utr3_df), function(ii){
    nchar(utr3_df[ii,1])
    })
    
  
  utr3_df1 <- cSplit(utr3_df, 2, sep = '|', direction = 'wide') %>% as.data.frame(stringsAsFactor = F)
  out <- utr3_df1[,c('name_1', 'len')] %>% unique() %>% 
    group_by(name_1) %>% 
    summarise(len = max(len))

}

utr3_df <- seq_proc(utr3_seq)
utr5_df <- seq_proc(utr5_seq)
cds_df <- seq_proc(cds_seq)



ensg_mapping3 <- ensg_mapping2 %>% 
  left_join(utr3_df, by = c('ensg'= 'name_1')) %>% rename(utr3_len = len) %>% 
  left_join(utr5_df, by = c('ensg'= 'name_1')) %>% rename(utr5_len = len) %>% 
  left_join(cds_df, by = c('ensg'= 'name_1')) %>% rename(cds_len = len) %>% 
  mutate(utr3_density = utr3_degree/utr3_len,
    utr5_density=utr5_degree/utr5_len,
    cds_density = cds_degree/cds_len)

View(ensg_mapping3)
View(cds_df)
nrow(cds_df %>% unique())
nrow(utr3_df)
length(utr3_df[,1] %>% unique)
View(utr3_df)
