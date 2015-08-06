
infomol <- read_csv('./raw_data/tcmsp-infomol.csv')
ob <- infomol$ob %>% as.data.frame()
ob <- ob %>% 
  filter(ob <100) 

# ob ----
ob <- infomol$ob %>% as.data.frame()
colnames(ob) <- 'ob'
data <- ob %>% 
  filter(ob <100) 
threshold <- 30
colname <- 'ob'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Oral bioavailability'
require(ggplot2)
data <- as.data.frame(data) 
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]<threshold, 0, 1)

g <- ggplot(data = data, aes(x = ob)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, binwidth = 5, na.rm = T ) 
g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = '#3399CC', '0' = '#99CCCC'), name = 'value', labels = c('<=30%', '>30%')) +
  scale_x_continuous(breaks = seq(0,100,10),limits = c(0,110) , expand = c(-0.05,0))+
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.9, 0.85))+
  scale_y_continuous(limits = c(0, 1800),expand = c(0, 0)) +
  xlab(title)+
  ylab('number of molecules')
  #ggtitle('Distribution')

ob_tu <- g1



# dl----
data <- infomol$dl
threshold <- 0.18
colname <- 'dl'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Drug-likeness'
  require(ggplot2)
  data <- as.data.frame(data) 
  colnames(data)[1] <- colname
  data$flag = ifelse( data[[colname]]<threshold, 0, 1)
g <- ggplot(data = data, aes_string(x = colname)) + 
    geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T, binwidth = 0.03 ) 
  
g1 <- g +
    theme_bw() +
    scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('<=', threshold), collapse = ''), 
                   paste(c('>', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
  scale_x_continuous(limits = c(0, 1), expand = c(-0.03,0))+
  scale_y_continuous(limits = c(0, 1500), expand = c(0, 0)) +
  xlab(title)+
  ylab('number of molecules')
  #ggtitle('Distribution')
  
dl_tu <- g1


# caco2----

data <- infomol$caco2
threshold <- -0.4
colname <- 'caco2'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Caco-2'
require(ggplot2)
data <- as.data.frame(data) 
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]<threshold, 0, 1)
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T, binwidth = 0.2) 

g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('<=', threshold), collapse = ''), 
    paste(c('>', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
  scale_x_continuous(limits= c(-6, 3), expand = c(-0.05, 0))+
  scale_y_continuous(limits = c(0, 1800), expand = c(0, 0)) +
  xlab(title)+
  ylab('number of molecules')
  #ggtitle('Distribution')

caco2_tu <- g1


# bbb----

data <- infomol$bbb
#threshold <- -0.4
colname <- 'bbb'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Blood brain barrier'
require(ggplot2)
data <- as.data.frame(data) 
colnames(data)[1] <- colname

data$flag = sapply(data[,1], function(x){
  ifelse(x > -0.3 & x < 0.3, 1, 0) 
})
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T , binwidth = 0.3) 

g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c('<=-0.3&>=0.3','>-0.3&<0.3')) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
  scale_x_continuous(limits= c(-8, 3), breaks = seq(-8, 3, 1.5), expand = c(-0.05, 0))+
  scale_y_continuous(limits = c(0, 1500), expand = c(0, 0)) +
  xlab(title)+
  ylab('number of molecules')
  #ggtitle('Distribution')

bbb_tu <- g1

# multiple graph ----
ob_tu <- ob_tu + theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))

dl_tu <- dl_tu + theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))

caco2_tu <- caco2_tu + theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))
bbb_tu <- bbb_tu + theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))


library(gridExtra)
# png('rjlo1.png', width = 1200, height = 1000)
#main <- textGrob('title', gp=gpar(cex = 1.2), vjust = 1)
#left <- textGrob('left', gp=gpar(cex = 1.2), vjust = 3)
png("plot01.png", width=35, height=20, res=300, units = 'cm')
grid.arrange(ob_tu, dl_tu, caco2_tu, bbb_tu)
#bbb_tu+theme(plot.margin = unit(c(0.1, 0, 0, 0),"cm"))
#
dev.off()

