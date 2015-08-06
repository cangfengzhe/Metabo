infomol <- read_csv('./raw_data/tcmsp-infomol.csv')


# half-life----
data <- infomol$halflife
#threshold <- 0
colname <- 'hl'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Drug half-life'
require(ggplot2)
data <- as.data.frame(data) %>% na.omit() 
colnames(data)[1] <- colname
data <- data %>% filter(hl>0)
data$flag = ifelse( data[[colname]]>=4 & data[[colname]]<= 8, 1, 0)

# data$flag = 0
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = as.factor(flag)), colour="white", stat='bin', na.rm = T, binwidth = 1) +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c('<4&>8', '>=4&<=8'))

g1 <- g +
  theme_bw() +
  #scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('<=', threshold), collapse = ''), 
    #paste(c('>', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
#   scale_x_continuous(limits = c(0, 1), expand = c(-0.03,0))+
#   scale_y_continuous(limits = c(0, 1500), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.0001, 30), ylim = c(0, 1200))+
  theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  xlab(title)+
  ylab('number of molecules')
#ggtitle('Distribution')
g1
hl_tu <- g1



# molecule weight----
data <- infomol$mw
threshold <- 500
colname <- 'mw'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Molecular weight'
require(ggplot2)
data <- as.data.frame(data) %>% na.omit()
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]>=threshold, 0, 1)
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T , binwidth = 50) 

g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('>=', threshold), collapse = ''), 
    paste(c('<', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
#   scale_x_continuous(limits = c(0, 1600), expand = c(-0.03,0))+
#   scale_y_continuous(limits = c(0, 1800), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.01, 1600), ylim = c(0, 1800))+
  theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  xlab(title)+
  ylab('number of molecules')
g1
#ggtitle('Distribution')

mw_tu <- g1




# hdon----
data <- infomol$hdon
threshold <- 5
colname <- 'hdon'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Hydrogen-bond donor'
require(ggplot2)
data <- as.data.frame(data) %>% na.omit()
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]>=threshold, 0, 1)
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',na.rm = T, binwidth = 1 ) 

g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('>=', threshold), collapse = ''), 
    paste(c('<', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
   #scale_x_continuous(breaks = seq(0, 20, 1))+
#   scale_y_continuous(limits = c(0, 4000), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.001, 20), ylim = c(0, 4000))+
  theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  
  xlab(title)+
  ylab('number of molecules')
g1
#ggtitle('Distribution')

hdon_tu <- g1


# hacc----
data <- infomol$hdon
threshold <- 10
colname <- 'hacc'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Hydrogen-bond acceptor'
require(ggplot2)
data <- as.data.frame(data) %>% na.omit()
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]>=threshold, 0, 1)
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T ) 
#g
g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('>=', threshold), collapse = ''), 
    paste(c('<', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
  #scale_x_continuous(breaks = seq(0, 20,2))+
#  scale_y_continuous(limits = c(0, 4000), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.01, 20), ylim = c(0, 4000))+
  theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  xlab(title)+
  ylab('number of molecules')
  
g1
#ggtitle('Distribution')

hacc_tu <- g1




# alogp----
data <- infomol$alogp
# threshold <- 10
colname <- 'alogp'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'AlogP'
require(ggplot2)
data <- as.data.frame(data) %>% na.omit()
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]>-2 & data[[colname]]< 5, 1, 0)
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T, binwidth = 1 ) 
#g
g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c('<=-2&>=5', '>-2&<5')) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
  # scale_x_continuous( expand = c(-0.00001,0))+
  # scale_y_continuous(limits = c(0, 2500), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-8, 20), ylim = c(0, 2500))+
  theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  xlab(title)+
  ylab('number of molecules')

g1
#ggtitle('Distribution')

alogp_tu <- g1

# RBN----
data <- infomol$rbn
threshold <- 10
colname <- 'rbn'
color1 = '#3399CC'
color2 = '#99CCCC'
title <- 'Number of rotatable bonds'
require(ggplot2)
data <- as.data.frame(data) %>% na.omit()
colnames(data)[1] <- colname
data$flag = ifelse( data[[colname]]>= threshold, 0, 1)
g <- ggplot(data = data, aes_string(x = colname)) + 
  geom_bar(aes(fill = factor(flag)),colour="white", stat='bin',width = 0, na.rm = T , binwidth = 1) 
#g
g1 <- g +
  theme_bw() +
  scale_fill_manual(values = c('1' = color1, '0' = color2), name = 'value', labels = c(paste(c('>=', threshold), collapse = ''), 
    paste(c('<', threshold), collapse = ''))) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position=c(0.90, 0.85))+
  #scale_x_continuous( expand = c(0,0))+
  #scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.01, 35), ylim = c(0, 2000))+
  theme( axis.title = element_text(size= 18),  axis.text = element_text(size = 14), legend.title = element_text(size = 12), legend.text = element_text(size = 12))+
  xlab(title)+
  ylab('number of molecules')

g1
#ggtitle('Distribution')

rbn_tu <- g1


# bind the graph ----
library(gridExtra)
png("plot02.png", width=35, height=30, res=300, units = 'cm')
grid.arrange(mw_tu, hdon_tu, hacc_tu, rbn_tu, alogp_tu, hl_tu )
#bbb_tu+theme(plot.margin = unit(c(0.1, 0, 0, 0),"cm"))
#
dev.off()
