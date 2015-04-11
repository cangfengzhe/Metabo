# calculate the CG content
library(seqinr)
library(splitstackshape)
# cds sequence from ensemble----
cds_raw <- seqinr::read.fasta('./raw_data/cds.fasta', as.string = T)
cds <- do.call('rbind', cds_raw) %>% 
  as.data.frame()
cds$name <- rownames(cds)
cds <- filter(cds, V1 != 'sequence unavailable') %>% 
  cSplit( splitCols = 2, sep = '|', direction = 'wide') %>% 
  select(c(2, 3, 1)) %>% 
  dplyr::rename(name = name_1, length = name_2, seq = V1)


# calculate the GC content
cds$name <- as.character(cds$name)
cds$seq <- as.character(cds$seq)
cds$gc <- sapply(cds$seq, function(x){
  GC(s2c(x))
})



# process the cds_utr data from the PARS website ----
cds_utr_raw <- seqinr::read.fasta('/Users/lipidong/work/protein\ bundunce/data/sce_gene_cds_utr.fasta', as.string = T) %>% 
  do.call('rbind', .) %>% 
  as.data.frame()
# 增加PARS爬取的数据
cds_utr_add <- seqinr::read.fasta('./data/unknow.fasta', as.string = T) %>% 
  do.call('rbind', .) %>% 
  as.data.frame()
cds_utr_raw <- rbind(cds_utr_raw, cds_utr_add)

cds_utr <- data.frame(name = rownames(cds_utr_raw),
                      length = nchar(as.character(cds_utr_raw[,1])),
                      seq = as.character(cds_utr_raw[,1]),
                      stringsAsFactors = F)

# calculate the gc content
cds_utr$name <- as.character(cds_utr$name)
cds_utr$seq <- as.character(cds_utr$seq)
cds_utr$gc <- sapply(cds_utr$seq, function(x){
  GC(s2c(x))
})

# data <- all_data
# data <- sqldf('select data.*, cds.length as cds_length, cds.gc as cds_gc, cds_utr.length as cds_utr_length, cds_utr.gc as cds_utr_gc from data left join cds on cds.name = data.sys_name left join cds_utr on cds_utr.name = data.sys_name')
# data <- rename(data, utr5_pars = utr5, utr3_pars = utr3, cds_pars = cds, intron_count = count, intron_len_sum = len_sum, translation_rate = translation)
# 
# all_data <- inner_join(data, sgd,  by = c('sgd_id' = 'X1')) %>% 
#   select(c(-2, -3))
# all_data <- select(data, -c(2,3)) %>% rename(sgd_id = X1)
# 
# has_intron <- filter(all_data, is.na(intron_count) == F) 
# 
# 
# write.csv(all_data, file = './data/yeast_result_0410.csv')
# 


# process the introne data----
# 转录子加上两侧UTR, ensembl,ncbi中转录子不含utr
# transcript
transcript <- read_csv('./raw_data/yeast_transcript.csv') %>%  
  select(1:4)
colnames(transcript) <- c('name', 'chrome', 'tran_start', 'tran_end')

# 转到 genome.R 处理 yeast chromosome data
source('./genome.R')
# 将罗马数字转换成阿拉伯
roman2ala <- unique(transcript$chrome) %>% 
  as.data.frame()

roman2ala$num <- c(8, 16, 15, 4, 7,3,1, 13, 12, 9, 5, 11, 10, 2, 14, 6, 'x' )
colnames(roman2ala) <- c('chrome', 'num')
View(roman2ala)

transcript <- transcript %>% 
  left_join(roman2ala, by = 'chrome')

transcript$seq <- sapply(1: nrow(transcript), function(ii){
  print(ii)
  extract_seq(transcript[ii,3], transcript[ii,4], transcript[ii,5])
})

transcript <- mutate(transcript, len = tran_end-tran_start+1, transcript_gc)

transcript_out <- transcript[,c(2, 7, 6)]
transcript_out <- mutate
utr3 <- read_csv("./data/utr3_pos.csv", col_names = F)
utr5 <- read_csv("./data/utr5_pos.csv", col_names = F) 
View(utr3)


