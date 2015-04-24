library(ggplot2)

niao_data <- read_csv('./data/xue_result.csv') 
data <- niao_data
colnames(data) <- c('ID', 'pvalue', 'FDR', 'FC')
data$fc <- sapply(data$FC, function(x){
 ifelse(x>1, x, -1/x) 
})

data_1 <- data %>% filter(fc>0) %>% arrange(fc)
data_2 <- data %>% filter(fc<0) %>% arrange(desc(fc))
data <- rbind_all(list(data_1, data_2))

data$ID <- factor(data$ID, levels = unique(data$ID))

g <- ggplot(data, aes(x = ID, y = fc, fill = factor(sapply(data$fc, function(fc){
  ifelse(fc>0,'qixu', 'pingshi')
  
}), levels = c('qixu', 'pingshi'))
                      )) + 
  geom_bar(stat = 'identity') +
  ggtitle("Average bill for 2 people") +     # Set title
  theme_classic() 
 g <- g+ theme(axis.ticks.y = element_blank())

(g <- g+  scale_fill_manual(values = c('#5cacee', '#f4a460'),
                      name = 'type',
                       breaks = c('pingshi', 'qixu' ),
                      labels = c('pingshi','qixu')) +
   coord_flip() +  # 反转
  scale_y_continuous(breaks=c(-2,-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2))
)

ggsave('xue.png',height = 20, units = 'cm')
 # library(plotly)
dev.off() 
 
