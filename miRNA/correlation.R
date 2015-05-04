# calculate the correlation 
corr_f <- function(data, rmcol){
  data <- data[,-rmcol]
  colnum <- ncol(data)
  corr <- matrix(NA, colnum, colnum)
  colnames(corr) <- colnames(data[,1:colnum])
  rownames(corr) <- colnames(data[,1:colnum])
  sapply(1: colnum, function(ii){
    sapply(1: colnum, function(jj){
      print(ii)
      print(jj)
      df <- data.frame(data[,ii],data[,jj])
      # df <- data.frame(log(data[,ii+2]),log(data[,jj+2])) 
      df <- na.omit(df)
      aa <- cor.test(df[,1], df[, 2], method = 'spearman')
        # estimate <- signif(aa$estimate,3)
        # pvalue <- signif(aa$p.value, 3)
        corr[ii,jj] <<- aa$estimate
        # paste(c(estimate, '/',pvalue), collapse = '')
      
      
     })
  })
   corr
}
corr <- 1
bb <- ensg_mapping %>% na.omit()
aa <- ensg_mapping[,-c(1,14,15)]
View(aa)
aa <- filter(aa, is.na(utr3_degree) ==F & is.na(utr5_degree) ==F & is.na(cds_degree) ==F)
nrow(aa)
df <- data.frame(ensg_mapping$utr5_degree, ensg_mapping$pro_abundance, ensg_mapping$utr3_degree) %>% na.omit()
ensg_essen %>% View
cor(house_keep_ensg[,-1], use = 'pairwise.complete.obs', method = 'sp') %>% View()


house_keep_ensg %>% View


pcor.test(df[,1],df[,2], df[,3], method = 'spearman')
