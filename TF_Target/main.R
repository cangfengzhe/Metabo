# main 
# mapping.R

all_data <- sqldf("select distinct SGD.sgd_id, SGD.sys_name,in_degree.in_degree,dn_ds_sgd.ds, dn_ds_sgd.dn,  dn_ds_sgd.dn_ds_adj, mRNA_hl_sgd.mRNA_halflife,  mRNA_abun_sgd.mRNA_abun_sum as mRNA_abun_mean, pars_sgd.mean as pars,protein_hl_sgd.hl_min as protein_halflife, protein_abun_sgd.abundance as protein_abundance, mf_all_seq_sgd.mf as all_seq_mf, mf_cds_sgd.cds_mf as cds_mf,mf_utr5_sgd.utr5_mf, mf_utr3_sgd.utr3_mf\nfrom SGD left join pars_sgd on SGD.sgd_id = pars_sgd.sgd_id\n                  left join protein_abun_sgd on protein_abun_sgd.sgd_id = SGD.sgd_id \n                  left join mf_all_seq_sgd on mf_all_seq_sgd.sgd_id = SGD.sgd_id\n                  left join mf_cds_sgd on mf_cds_sgd.sgd_id = SGD.sgd_id \n                  left join mf_utr5_sgd on mf_utr5_sgd.sgd_id = SGD.sgd_id\n                  left join mf_utr3_sgd on mf_utr3_sgd.sgd_id = SGD.sgd_id   left join mRNA_hl_sgd on mRNA_hl_sgd.sgd_id = SGD.sgd_id   left join dn_ds_sgd on dn_ds_sgd.sgd_id = SGD.sgd_id   left join in_degree on in_degree.name = SGD.sgd_id   left join protein_hl_sgd on protein_hl_sgd.sgd_id = SGD.sgd_id  left join mRNA_abun_sgd on mRNA_abun_sgd.sgd_id = SGD.sgd_id")


all_data <- left_join(all_data, trans_rate, by = c('sys_name' = 'sys_name')) %>% 
  select(-23,-16)

colnames(all_data)[16] <- 'mRNA_amount'
colnames(all_data)[17] <- 'mRNA_stability'
colnames(all_data)[18] <- 'Transcription_rate_indirect'
colnames(all_data)[19] <- 'Transcription_rate'

# [Di+(ln2)/T]Pi/Mi
all_data <- mutate(all_data, translation = (log(2)/protein_halflife + log(2)/90) * protein_abundance/mRNA_abun_mean)

all_data <- left_join(all_data,proc_pars, by = c('sys_name' = 'name'))
all_data <- select(all_data,c(-9))
colnames(all_data)[22] <- 'pars'

# join the introne to the all_data
# source(extract_intron.r)
all_data <- left_join(all_data, intron_out, by = c('sys_name' = 'V1'))


# source(cg_content.r)
data <- all_data
data <- sqldf('select data.*, cds.length as cds_length, cds.gc as cds_gc, cds_utr.length as cds_utr_length, cds_utr.gc as cds_utr_gc from data left join cds on cds.name = data.sys_name left join cds_utr on cds_utr.name = data.sys_name')

data <- rename(data, utr5_pars = utr5, utr3_pars = utr3, cds_pars = cds, intron_count = count, intron_len_sum = len_sum, translation_rate = translation)



# source(filter_gene.r)
all_data <- inner_join(data, sgd,  by = c('sgd_id' = 'X1')) %>% 
  select(c(-2, -3))

all_data <- select(data, -c(2,3)) %>% rename(sgd_id = X1)

has_intron <- filter(all_data, is.na(intron_count) == F) 


write.csv(all_data, file = './data/yeast_result_0410.csv', row.names = F)
# source(correlation.r)


# source(gc_content.r)

intron_list <- filter(all_data, is.na(intron_count) == F)[,c(2, 26, 27)] %>% unique()

intron_seq <- intron_list %>% 
  left_join(transcript, by = c('sys_name' = 'name')) %>% distinct() %>% 
  na.omit()

intron_seq$length <- sapply(1:nrow(intron_seq), function(ii){
  intron_seq[ii,3] %>% nchar() - intron_seq[ii,4] %>% nchar()
})


intron_seq <- sqldf('select intron_seq.*, intron_out.count, intron_chrome.seq from intron_seq left join intron_chrome on intron_seq.sys_name = intron_chrome.V1 left join intron_out on intron_out.V1 = intron_seq.sys_name') %>% select(c(1,6,2, 3, 4, 7))

intron_seq_1 <- filter(intron_seq, count == 1) %>% arrange(desc(intron_len_sum)) %>% 
mutate(hasintron_length = nchar(transcript_seq), nointron_length = nchar(mrna_seq), inton_length = nchar(seq)) %>% select(c(1,2,7:9,4:6))%>% 
  dplyr:: rename(intron_count = count, hasintron_seq = transcript_seq, nointron_seq = mrna_seq, intron_seq = seq)

# 内含子的互补配对问题-----
sapply(1:nrow(intron_seq_1), function(ii){
  print(ii)
  flag1 <- grepl(intron_seq_1[ii,8], intron_seq_1[ii,6])
  if(!flag1){
    flag2 <- grepl(intron_seq_1[ii,8] %>% DNAString() %>% reverseComplement() %>% tolower(), intron_seq_1[ii,6])
    if(flag2){
      intron_seq_1[ii,8] <<- intron_seq_1[ii,8] %>% DNAString() %>% reverseComplement() %>% tolower()
    }else{
      intron_seq_1[ii,8] <<- 'error'
    }
    
  }
  
  })




write_csv(intron_seq_1, path = './data/intron_seq.csv')

# 生成fasta文件由RNAfold 处理
ii = 0
sapply(c(1:5,51:55, 221:225), function(ii){
  
  has_intron <- paste(c('>', intron_seq_1[ii,1],'_hasintron_', intron_seq_1[ii, 3], '\n'), collapse = '')
  cat(has_intron, file = './data/fasta/seq.fasta', append = T)
  #cat('\n', file = './data/fasta/seq.fasta', append = T)
  cat(intron_seq_1[ii,6], file = './data/fasta/seq.fasta', append = T)
  cat('\n', file = './data/fasta/seq.fasta', append = T)
  
  no_intron <- paste(c('>', intron_seq_1[ii,1], '_nointron_', intron_seq_1[ii, 4], '\n'), collapse = '')
  cat(no_intron, file = './data/fasta/seq.fasta', append = T)
  # cat('\n', file = './data/fasta/seq.fasta', append = T)
  cat(intron_seq_1[ii,7], file = './data/fasta/seq.fasta', append = T)
  cat('\n', file = './data/fasta/seq.fasta', append = T)
  
  
  intron <- paste(c('>', intron_seq_1[ii,1], '_intron_', intron_seq_1[ii, 5], '\n'), collapse = '')
  cat(intron, file = './data/fasta/seq.fasta', append = T)
  # cat('\n', file = './data/fasta/seq.fasta', append = T)
  cat(intron_seq_1[ii,8], file = './data/fasta/seq.fasta', append = T)
  cat('\n', file = './data/fasta/seq.fasta', append = T)
  
})


save.image('./data/all_data.rdata')
