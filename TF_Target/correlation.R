# calulate the correlation
data <- has_intron
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
      corr[ii,jj] <<- aa$estimate
      # paste(c(estimate, '/',pvalue), collapse = '')
    })
    
    # cor.test(data[,ii+2],log2(data[,jj+2]), method = 'spearman')$estimate
    
    
  })
})


View(corr)

save.image('./data/data.rdata')

write.csv(data, file = './data/yeast_intron_result_0410.csv', row.names = F)
write.csv(corr, file = './data/yeast_intron_corr_0410.csv')


