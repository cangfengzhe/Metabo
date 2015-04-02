library(igraph)
library(dplyr)

all_data_degree <- sqldf("select * from all_data left join in_degree on in_degree.name = all_data.sgd_id")
all_data <- all_data_degree[, c(1,2,12,5:11)]
save.image('./data/all_data.rdata')
corr1 <- corr
corr <- matrix(NA, 8,8)
colnames(corr) <- colnames(all_data[,3:10])
rownames(corr) <- colnames(all_data[,3:10])
sapply(1:8, function(ii){
  sapply(1:8, function(jj){
    print(ii)
    print(jj)
    df <- data.frame(all_data[,ii+2],all_data[,jj+2]) 
    df <- na.omit(df)
  corr[ii,jj] <<-  cor.test(df[,1], df[, 2], method = 'spearman')$estimate
    # cor.test(all_data[,ii+2],log2(all_data[,jj+2]), method = 'spearman')$estimate

    
  })
})
ii=1
jj=2

write.csv(all_data, file = './data/yeast_result.csv', row.names = F)
write.csv(corr1, file = './data/yeast_correlation.csv')
