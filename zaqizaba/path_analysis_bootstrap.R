# bootstrap analysis
library(sem)
library(dplyr)
library(readr)
yeast_data <- read_csv('./raw_data/yeast_data_halflife_2015.csv')
cor(data.frame(yeast_data$pars, yeast_data$mRNA_amount) %>% na.omit, method = 'spear')
# yeast_data <- yeast_data[, c('mRNA_halflife', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015')] %>% na.omit()

raw_data <- yeast_data[, c('pars', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015')] %>% na.omit() %>% as.data.frame()


path_analysis <- function(str_cor){
  # 通径分析函数
  # str_cor: 相关性矩阵中, 提取下三角数据合并为字符串
  R.kerch <- readMoments(diag=FALSE, names=c('pars', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015'), text = str_cor)
  model.kerch <- specifyModel(text = '
                              pars -> mRNA_amount, a, NA
                              pars -> Transcription_rate, b, NA
                              pars -> mRNA_half_life2015, c, NA
                              Transcription_rate -> mRNA_half_life2015, ccc, NA
                              Transcription_rate -> mRNA_amount, d, NA
                              
                              mRNA_half_life2015 -> mRNA_amount, e, NA
                              pars <-> pars, f, NA
                              mRNA_amount <-> mRNA_amount, g, NA
                              Transcription_rate <-> Transcription_rate, h, NA
                              mRNA_half_life2015 <-> mRNA_half_life2015, i,NA
                              ')
#mRNA_half_life2015 <-> mRNA_half_life2015, i, NA
sem.kerch <- sem(model.kerch, R.kerch, nrow(raw_data))
summ <- summary(sem.kerch)
coeff <- summ$coeff
# 生成通径图
# pathDiagram(sem.kerch, edge.labels="values")
# 返回值为  
# coeff[1,1] : mRNA_amount <--- pars
# coeff[2,1]:  Transcription_rate <--- pars
# coeff[5,1]: mRNA_amount <--- Transcription_rate
# coeff[4,1]: mRNA_half_life2015 <--- Transcription_rate
# coeff[6,1]: mRNA_amount <--- mRNA_half_life2015
# coeff[3,1]: mRNA_half_life2015 <--- pars
return(c(coeff[1,1], coeff[2,1]*coeff[5,1]+coeff[2,1]*coeff[4,1]*coeff[6,1],
  coeff[3,1]*coeff[6,1]))

}
# path_analysis(str_cor)

# bootstrap
out <- ldply(1:10000,.progress = 'text', function(ii){
  sample_data <- sample_n(raw_data, nrow(raw_data), replace = T)
  cor_num <- cor(sample_data, method = 'spear')
  str_cor <- paste(cor_num[2,1], cor_num[3,1], cor_num[3,2], cor_num[4,1],cor_num[4,2],cor_num[4,3])
  path_analysis(str_cor)
})
colnames(out) <- c('pars2mRNA_amount', 'pars2Transcript_rate2mRNA_amount', 'pars2mRNA_half_life2mRNA_amount')
# save data
save(out, file = 'path_analysis_bootstrap.rda')

len <- length(out[which((out[,2]-out[,3])<0),] )
write_csv(out, './data/path_analysis_bootstrap.csv')
