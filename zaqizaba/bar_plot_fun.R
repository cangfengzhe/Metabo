g_tu <- function(data, colname, threshold, color1 = '#3399CC', color2 = '#99CCCC', title){
  
  require(ggplot2)
  data <- as.data.frame(data) 
  colnames(data)[1] <- colname
  data$flag = ifelse( data[[colname]]<threshold, 0, 1)
  g <- ggplot(data = data, aes_string(x = colname)) + 
    geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T ) 
  
  g <- g +
    theme_bw() +
    scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'ob', labels = c(paste(c('<', threshold), collapse = ''), 
      paste(c('>=', threshold), collapse = ''))) +
    scale_x_continuous(breaks = seq(0,100,10),limits = c(0,110) , expand = c(-0.05,0))+
    theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position=c(0.93, 0.85))+
    scale_y_continuous(limits = c(0, 1800),expand = c(0, 0)) +
    ggtitle(title)
  
  g
}


