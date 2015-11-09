# bootstrap analysis
library(sem)
library(dplyr)
library(readr)
yeast_data <- read_csv('./raw_data/yeast_data.csv')
colnames(yeast_data)
cor(data.frame(yeast_data$pars, yeast_data$mRNA_amount) %>% na.omit, method = 'spear')
# yeast_data <- yeast_data[, c('mRNA_halflife', 'mRNA_amount', 'Transcription_rate', 'mRNA_half_life2015')] %>% na.omit()

# 需要的变量
# pars, mHL mRNA_half_life2015, CAI, TcR Transcription_rate , mAb mRNA_amount, TLR translation_rate, phL protein_halflife, pAb protein_abundance

raw_data <-
  yeast_data[, c(
    'pars', 'mRNA_half_life2015', 'CAI',  'Transcription_rate', 'mRNA_amount', 'translation_rate', 'protein_halflife', 'protein_abundance'
  )] %>% na.omit() %>% as.data.frame()

nrow(raw_data)
# Create log column

tmp_data <- raw_data[,-1] %>% log2()
raw_data <- cbind(raw_data$pars, tmp_data)
colnames(raw_data)[1] <- 'pars'

# path_analysis----
path_analysis <- function(str_cor) {
  # 通径分析函数
  
  # str_cor: 相关性矩阵中, 提取下三角数据合并为字符串
  R.kerch <- 
    readMoments(diag = FALSE, names = colnames(raw_data), text = str_cor)
  
  # pars <- CAI, b, NA
  model.kerch <- specifyModel(
    text = '
    pars -> CAI, a, NA
    pars <- CAI, b, NA
    pars -> mRNA_half_life2015, c, NA
    pars -> Transcription_rate, d, NA
    pars -> translation_rate, e, NA
    CAI -> mRNA_half_life2015, f, NA
    CAI -> Transcription_rate, g, NA
    CAI -> translation_rate, h, NA
    mRNA_half_life2015 -> mRNA_amount, i, NA
    Transcription_rate -> mRNA_amount, j, NA
    Transcription_rate -> mRNA_half_life2015, jj, NA
    mRNA_amount -> protein_abundance, k, NA
    translation_rate -> protein_abundance, l, NA
    translation_rate -> mRNA_half_life2015, m, NA
    translation_rate -> protein_halflife, n, NA
    protein_halflife -> protein_abundance, o, NA
    pars <-> pars, p, NA
    CAI <-> CAI, q, NA
    mRNA_half_life2015 <-> mRNA_half_life2015, r, NA
    translation_rate <-> translation_rate, s, NA
    mRNA_amount <-> mRNA_amount, t, NA
    Transcription_rate <-> Transcription_rate, u, NA
    protein_halflife <-> protein_halflife, v, NA
    protein_abundance <-> protein_abundance, w, NA
    '
  )
  #mRNA_half_life2015 <-> mRNA_half_life2015, i, NA
  sem.kerch <- suppressMessages(suppressWarnings(sem(model.kerch, R.kerch, nrow(raw_data))))

  # 生成通径图
  return(sem.kerch)
  
}
# path_analysis
cor_num <- cor(raw_data, method = 'spearman')
str_cor <- paste(cor_num[upper.tri(cor_num)], collapse = ' ')
out <- path_analysis(str_cor)
col_name <- out$semmod[,1]

# bootstrap----
out <- ldply(1:10000, function(ii) {

  sample_data <- sample_n(raw_data, nrow(raw_data), replace = T)
  cor_num <- cor(sample_data, method = 'spear')
  str_cor <- paste(cor_num[upper.tri(cor_num)], collapse = ' ')
  result <- suppressMessages(path_analysis(str_cor))
  result$coeff[1:16]
})
colnames(out) <- col_name[1:16]
mean_val <- colMeans(out)

# save data
save(out, file = 'path_analysis_bootstrap.rda')

len <- length(out[which((out[,2] - out[,3]) < 0),])
write_csv(out, './data/path_analysis_bootstrap2.csv')
