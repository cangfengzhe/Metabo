niao_data <- read_csv('./data/niao_result.csv') 
colnames(niao_data) <- c('ID', 'pvalue', 'FDR', 'FC')
niao_data$fc <- sapply(niao_data$FC, function(x){
 ifelse(x>1, x, -1/x) 
})

library(ggplot2)

g <- ggplot(niao_data, aes(x = ID, y = fc, fill = function(fc){ifelse(fc>0,'red', 'blue')})) + 
  geom_bar(stat = 'identity') +
  ggtitle("Average bill for 2 people") +     # Set title
  theme_classic() 
g <- g+ theme(axis.ticks.y = element_blank())

g+  scale_color_manual(values=c('blue', 'red')) +
  coord_flip() 
  


