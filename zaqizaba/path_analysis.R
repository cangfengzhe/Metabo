yeast_data <- read_csv('./raw_data/yeast_data_halflife_2015.csv')
colnames(yeast_data)
# yeast_data <- yeast_data[, c('mRNA_halflife', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015')] %>% na.omit()
library(sem)
raw_data <- yeast_data[, c('pars', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015')] %>% na.omit() %>% as.data.frame()
cor(raw_data, method = 'spear')

# 得到的相关性写入下公式
R.kerch <- readMoments(diag=FALSE, names=c('pars', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015'), text = '0.4507096  0.3476375   0.4370306 0.2199192   0.3447967 0.02934686')



model.kerch <- specifyModel(text = '
pars -> mRNA_amount, a, NA
pars -> Transcription_rate, b, NA
pars -> mRNA_half_life2015, c, NA
Transcription_rate <-> mRNA_half_life2015, ccc, NA
Transcription_rate -> mRNA_amount, d, NA
mRNA_half_life2015 -> mRNA_amount, e, NA
pars <-> pars, f, NA
mRNA_amount <-> mRNA_amount, g, NA
Transcription_rate <-> Transcription_rate, h, NA
mRNA_half_life2015 <-> mRNA_half_life2015, i,NA
')
#mRNA_half_life2015 <-> mRNA_half_life2015, i, NA

    
sem.kerch <- sem(model.kerch, R.kerch, 1705)
summ <- summary(sem.kerch)

pathDiagram(sem.kerch, edge.labels="values")

write_csv(summ$coeff, './data/path_analysis.csv')


specify.model()


# 通径分析
library(agricolae)
corr.x <- cor(raw_data[, -2], method = 'spear')
corr.y <- rbind(cor(raw_data$pars, raw_data$mRNA_amount, , method = 'spear'), 
          cor(raw_data$mRNA_amount, raw_data$Transcription_rate, method = 'spear'), 
          cor(raw_data$mRNA_amount, raw_data$mRNA_half_life2015, method = 'spear'))
path.analysis(corr.x, corr.y)
