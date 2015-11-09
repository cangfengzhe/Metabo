library(parallel)
library(BSDA)
library(dplyr)
detectCores(logical = F) #获得实际核数
cl <- makeCluster(getOption("cl.cores", 4))  # 设置并行核数为4

z_test <- function(x, y, alternative =  "two.sided"){
  out <- z.test(x, y, alternative = alternative, sigma.x = sqrt(var(x)), sigma.y = sqrt(var(y)))
  return(out)
}

bootstrap <- function(df){
  len <- nrow(df)
  cor_value <- sapply(1:1000, function(ii){
    samp <- sample_n(df, len, replace = T)
    cor1 <- cor(samp[, 1], samp[, 2], method = "spearman")
    cor2 <- cor(samp[, 1], samp[, 3], method = "spearman")
    c(cor1, cor2)
  })
  cor_value <- t(cor_value)
  out <- z_test(cor_value[,1], cor_value[,2])
  result <- c(mean(cor_value[, 1]), mean(cor_value[, 2]), median(cor_value[, 1]), median(cor_value[, 2]), out$p.value)
  print(sqrt(var(cor_value[,1])))
  print(sqrt(var(cor_value[,2])))
  return(result)
}

raw_data <- read_csv('./bootstrap.csv')
colnames(raw_data)
# PARS-TcR vs PARS-mRNAhalf life
pars_tcr_hl <- raw_data[, c(7, 5, 4)] %>% na.omit()

# PARS-TcR vs CAI-TcR
tcr_pars_cai <- raw_data[, c(5, 7, 9)] %>% na.omit()

# PARS-mRNAhalflife vs. CAI-mRNAhalflife
mRNAhl_pars_cai <- raw_data[, c(10, 7, 9)] %>% na.omit()
# PARS-mRNAhalflife vs. CAI-mRNAhalflife
mRNAhl_pars_cai <- raw_data[, c(10, 7, 9)] %>% na.omit()

out_pars_tcr_hl <- bootstrap(pars_tcr_hl)
out_tcr_pars_cai <- bootstrap(tcr_pars_cai)
out_mRNAhl_pars_cai <- bootstrap(mRNAhl_pars_cai)

# intron-PARS-TcR vs intronless-PARS-TcR

intron_pars_tcr <- raw_data[, c('intron_len_sum', 'pars', 'Transcription_rate')]
df <- intron_pars_tcr
len <- nrow(df)
cor_intron_pars_tcr <- sapply(1:1000, function(ii){
  samp <- sample_n(df, len, replace = T)
  intron_pars <- samp %>% filter(is.na(intron_len_sum) == F) %>% select(pars, Transcription_rate) %>% na.omit()
  nointron_pars <- samp %>% filter(is.na(intron_len_sum) == T) %>% select(pars, Transcription_rate) %>% na.omit()
  cor1 <- cor(intron_pars[, 1], intron_pars[, 2], method = "spearman")
  cor2 <- cor(nointron_pars[, 1], nointron_pars[, 2], method = "spearman")
  c(cor1, cor2)
})
cor_intron_pars_tcr <- t(cor_intron_pars_tcr)
test_intron_pars_tcr <- z_test(cor_intron_pars_tcr[,1], cor_intron_pars_tcr[,2])
out_intron_pars_tcr <- c(mean(cor_intron_pars_tcr[, 1]), mean(cor_intron_pars_tcr[, 2]), median(cor_intron_pars_tcr[, 1]), median(cor_intron_pars_tcr[, 2]), test_intron_pars_tcr$p.value)

cor_bootstrap <- data.frame(pars_tcr_hl=out_pars_tcr_hl,
                            tcr_pars_cai=out_tcr_pars_cai,
                            mRNAhl_pars_cai=out_mRNAhl_pars_cai,
                            intron_pars_tcr=out_intron_pars_tcr)
rownames(cor_bootstrap) <- c("cor1_mean", "cor1_median", "cor2_mean", "cor2_median","z_test_pvalue")
out <- t(cor_bootstrap)
write.csv(out, file = './data/cor_bootstrap.csv')
