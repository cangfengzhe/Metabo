tf_target <- read.csv("/Users/lipidong/work/protein bundunce/data/TF_target.csv", 
                      stringsAsFactors = F)
tf_list <- data.frame(name = unique(tf_target[, 1]), stringsAsFactors = F)
# clear the 'p' in the tail of the words
cutp <- function(x){
  len <- nchar(x)
  if(substr(x, len, len) == 'p'){
    out <- substr(x, 1, len-1)
     out
  }
  else{
    x
  }
}

sapply(1:nrow(tf_list), function(ii){
  tf_list[ii,2] <<- cutp(tf_list[ii,1])
})
# sqldf 采用sqlite数据库 大小写敏感

tf_sgd <- sqldf("select distinct tf_list.name,tf_list.V2, SGD.sgd_id, SGD.sys_name from tf_list left join SGD on LOWER(tf_list.V2) = LOWER(SGD.sys_name) OR LOWER(tf_list.V2) = LOWER(SGD.std_name) ")

library(igraph)
tf_target <- unique(tf_target_sgd[,c(1,3)]) %>% na.omit()
g <- graph.data.frame(tf_target)
out_degree <- degree(g, mode = 'out')
out_degree <- out_degree[unique(tf_target[,1])]
out_degree <- as.data.frame(out_degree)
out_degree$name <- rownames(out_degree)

out_degree <- left_join(out_degree, tf_sgd, by = c('name' = 'name')) %>% select(c(4,5,2))
colnames(out_degree) <- c('sgd_id','name','out_degree')
View(in_degree)

tf_data <- sqldf("select SGD.sgd_id, SGD.sys_name,out_degree.out_degree,dn_ds_sgd.ds, dn_ds_sgd.dn,  dn_ds_sgd.dn_ds_adj, mRNA_hl_sgd.mRNA_halflife,  mRNA_abun_sgd.mRNA_abun_sum as mRNA_abun_mean, pars_sgd.mean as pars,protein_hl_sgd.hl_min as protein_halflife, protein_abun_sgd.abundance as protein_abundance, mf_all_seq_sgd.mf as all_seq_mf, mf_cds_sgd.cds_mf as cds_mf,mf_utr5_sgd.utr5_mf, mf_utr3_sgd.utr3_mf from SGD left join pars_sgd on SGD.sgd_id = pars_sgd.sgd_id\n                  left join protein_abun_sgd on protein_abun_sgd.sgd_id = SGD.sgd_id \n                  left join mf_all_seq_sgd on mf_all_seq_sgd.sgd_id = SGD.sgd_id\n                  left join mf_cds_sgd on mf_cds_sgd.sgd_id = SGD.sgd_id \n                  left join mf_utr5_sgd on mf_utr5_sgd.sgd_id = SGD.sgd_id\n                  left join mf_utr3_sgd on mf_utr3_sgd.sgd_id = SGD.sgd_id   left join mRNA_hl_sgd on mRNA_hl_sgd.sgd_id = SGD.sgd_id   left join dn_ds_sgd on dn_ds_sgd.sgd_id = SGD.sgd_id   left join out_degree on out_degree.sgd_id = SGD.sgd_id   left join protein_hl_sgd on protein_hl_sgd.sgd_id = SGD.sgd_id  left join mRNA_abun_sgd on mRNA_abun_sgd.sgd_id = SGD.sgd_id")

# mRNA transcription rate----
# data from A Complete Set of Nascent Transcription Rates for Yeast Genes
trans_rate <- read.csv('./raw_data/transcript_rate.csv', stringsAsFactors = F)
View(trans_rate)

tf_data <- left_join(tf_data, trans_rate, by = c('sys_name' = 'sys_name')) %>% 
  select(-23,-16)
colnames(tf_data)[17] <- 'mRNA_amount'
colnames(tf_data)[18] <- 'mRNA_stability'
colnames(tf_data)[19] <- 'Transcription_rate_indirect'
colnames(tf_data)[20] <- 'Transcription_rate'
# [Di+(ln2)/T]Pi/Mi
tf_data <- mutate(tf_data, translation = (protein_halflife/60 + log(2)/90) * protein_abundance/mRNA_abun_mean)

tf_data <- left_join(tf_data,proc_pars, by = c('sys_name' = 'name'))

tf_data <- filter(tf_data, is.na(out_degree)==F)


colnum <- ncol(all_data)
corr <- matrix(NA, colnum-2, colnum-2)
colnames(corr) <- colnames(all_data[,3:colnum])
rownames(corr) <- colnames(all_data[,3:colnum])
sapply(1: (colnum-2), function(ii){
  sapply(1: (colnum-2), function(jj){
    print(ii)
    print(jj)
    df <- data.frame(all_data[,ii+2],all_data[,jj+2])
    # df <- data.frame(log(all_data[,ii+2]),log(all_data[,jj+2])) 
    df <- na.omit(df)
    try({
      
      aa <- cor.test(df[,1], df[, 2], method = 'spearman')
      # estimate <- signif(aa$estimate,3)
      # pvalue <- signif(aa$p.value, 3)
      corr[ii,jj] <<- aa$estimate
        # paste(c(estimate, '/',pvalue), collapse = '')
    })
    
  })
})
write.csv(tf_target, file = './data/yeast_TF_0406.csv', row.names = F)
write.csv(corr, file = './data/yeast_TF_corr_0406.csv')
