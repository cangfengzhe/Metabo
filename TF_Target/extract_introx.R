extron <- read.csv('./raw_data/extron.txt', stringsAsFactors = F)
colnames(extron) <- c('gene', 'transcript', 'tran_start', 'tran_end', 'entron_start', 'entron_end')
nrow(unique(extron))
# 减去转录子中外显子等于转录子的
intron <- setdiff(extron, filter(extron, tran_start == entron_start & tran_end == entron_end))

# 经过检查 一共有396个基因，都是内含子位于外显子之中
# 检查的方法为转录子起始位点与外显子起始位点相同的基因396个
# 转录子终止位点与外显子终止位点相同的基因396个
filter(intron, tran_end == entron_end) %>% View()
  select(1) %>% 
  distinct() %>% nrow()

unique(intron[,1]) %>% length()

# filter the intron
intron_gene <- unique(intron[,1])
intron_mat <- matrix(NA,1000,4)
index <- 0
aa <- sapply(intron_gene, function(x){
  y <- filter(intron, gene == x) %>% arrange(entron_start)
  sapply(1:(nrow(y) - 1), function(ii){
    index <<- index + 1
    start <- y[ii, 6] + 1
    end <- y[ii+1, 5] - 1
    len <- end - start + 1
    intron_mat[index, 1:4] <<- c(x, len, start, end)
  })
})

intron_df <- as.data.frame(intron_mat) %>% unique()
intron_df[,2] <- as.character(intron_df[, 2])
intron_df[,2] <- as.numeric(intron_df[, 2])

intron_out <- group_by(intron_df, V1) %>% 
  summarise(count = n(), len_sum=sum(V2))
# join to the all_data
all_data <- left_join(all_data, intron_out, by = c('sys_name' = 'V1'))


filter(all_data, is.na(count) == T) %>% select(RNA_pol_density, len_sum) %>%   na.omit() %>%  colMeans(na.rm = T)

# 计算差异
intron_diff <- data.frame()
sapply(1:(ncol(all_data)-4), function(ii){
  print(ii)
  y <- filter(all_data, is.na(count) == F)[,ii+2] %>% na.omit() %>%  as.numeric()
  n <- filter(all_data, is.na(count) == T)[,ii+2] %>% na.omit() %>% as.numeric()
  
  intron_diff[ii, 1] <<- colnames(all_data)[ii+2]
  intron_diff[ii, 2] <<- length(y) 
  
  # n <- sample(n, length(y))
  intron_diff[ii, 3] <<- length(n)
  intron_diff[ii, 4] <<- mean(y)
  intron_diff[ii, 5] <<- mean(n)
  
  intron_diff[ii, 6] <<- wilcox.test(y,n)$p.value
  }) 
View(intron_diff)


colnames(intron_diff) <- c('name', 'count_in_intron', 'count_in_nointron','mean_in_intron', 'mean_in_nointron','wilcox_pvalue')
View(intron_diff)
write.csv(intron_diff, file = './data/intron_diff.csv')


data <- no_intron
colnum <- ncol(data)
corr <- matrix(NA, colnum-2, colnum-2)
colnames(corr) <- colnames(data[,3:colnum])
rownames(corr) <- colnames(data[,3:colnum])
sapply(1: (colnum-2), function(ii){
  sapply(1: (colnum-2), function(jj){
    print(ii)
    print(jj)
    df <- data.frame(data[,ii+2],data[,jj+2])
    # df <- data.frame(log(data[,ii+2]),log(data[,jj+2])) 
    df <- na.omit(df)
    try({
      
      aa <- cor.test(df[,1], df[, 2], method = 'spearman')
      # estimate <- signif(aa$estimate,3)
      # pvalue <- signif(aa$p.value, 3)
      corr[ii,jj] <<- signif(aa$estimate,3)
      # paste(c(estimate, '/',pvalue), collapse = '')
    })
    
    # cor.test(data[,ii+2],log2(data[,jj+2]), method = 'spearman')$estimate
    
    
  })
})


has_intron <- filter(all_data, is.na(intron_count) == F)


write.csv(data, file = './data/yeast_has_intron_0408.csv', row.names = F)


n <- all_data[which(is.na(all_data$count)==T),]
View(n)
