library(ggplot2)
infomol <- read_csv('./raw_data/tcmsp-infomol.csv')
View(infomol)
ob <- infomol$ob %>% as.data.frame()
colnames(ob) <- 'ob'
ob <- ob %>% 
  filter(ob <100) %>% 
  mutate(flag = ifelse(ob<30, 0, 1))


g_tu <- function(data, colname){
  ob_tu <- ggplot(data = ob, aes(x = ob)) + 
    geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, binwidth = 5, na.rm = T ) 
  aa <- ob_tu +
    theme_bw() +
    scale_fill_manual(values = c('1' = '#3399CC', '0' = '#99CCCC'), name = 'ob', labels = c('<30', '>=30')) +
    scale_x_continuous(breaks = seq(0,100,10),limits = c(0,110) , expand = c(-0.05,0))+
    theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position=c(0.9, 0.85))+
    scale_y_continuous(limits = c(0, 1800),expand = c(0, 0)) 
  
  aa
}


multiplot(aa, aa, aa, aa, cols = 2)
grid.arrange(aa, aa)
hist
