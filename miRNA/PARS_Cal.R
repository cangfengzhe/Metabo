# data from GSE50676,human, calulate the PARS score
# method from nature
library(dplyr)
library(sqldf)

# parallel----
library(foreach) # nrow(S1_V1)
library(doMC)

registerDoMC(cores=2)
getDoParRegistered()
getDoParWorkers()
getDoParName()

V1 <- read_delim('./raw_data/Homo_PARS_GEO_Data/GSM1226162_norm.GM12892_V1.tab', delim = '\t', col_names = F) 

S1 <- read_delim("./raw_data/Homo_PARS_GEO_Data/GSM1226161_norm.GM12892_S1.tab", delim = '\t', col_names = F) 
# V1 <- read.delim('/Users/lipidong/work/protein
# bundunce/data/PARS_GEO_Data/sce_V1.tab', stringsAsFactors =
# F, header = F) S1 <- read.delim('/Users/lipidong/work/protein
# bundunce/data/PARS_GEO_Data/sce_S1.tab', stringsAsFactors =
# F, header = F) Score <-
# read.delim('/Users/lipidong/work/protein
# bundunce/data/PARS_GEO_Data/sce_Score.tab', stringsAsFactors
# = F, header = F) S1 V1 合并
S1_V1 <- left_join(S1, V1, by = 'X1') %>% as.data.frame()

colnames(S1_V1) <- c("name", "S1", "V1")
s1_data <- strsplit(S1_V1[, 2], ";")
v1_data <- strsplit(S1_V1[, 3], ";")

# normalize by sequence depth ----
# calulate the sequence depth
s1_tmp <- 0
s1_length <- NA
l_ply(1: length(s1_data), function(ii){
    s1_tmp <<-s1_tmp + sum(as.numeric(s1_data[[ii]]))
    s1_length[ii] <<- length(s1_data[[ii]])
}, .progress = 'text')
s1_depth <- s1_tmp/sum(s1_length)


v1_tmp <- 0
v1_length <- NA
l_ply(1: length(v1_data), function(ii){
  v1_tmp <<- v1_tmp+ sum(as.numeric(v1_data[[ii]]))
  v1_length[ii] <<- length(v1_data[[ii]])
}, .progress = 'text')
v1_depth <- v1_tmp/sum(v1_length)

# calulate the mean----

pars_cal <- function(ii) {
  # sapply(1:1000, function(ii){
  cat(ii)
  s1 <- as.numeric(s1_data[[ii]])/s1_depth
  v1 <- as.numeric(v1_data[[ii]])/v1_depth
  len <- length(s1)
  # first 2
  pars01 <- log2(v1[1] + 5) - log2(s1[1] + 5)
  pars02 <- log2(v1[2] + 5) - log2(s1[2] + 5)
  # last 2
  pars11 <- log2(v1[len] + 5) - log2(s1[len] + 5)
  pars12 <- log2(v1[len - 1] + 5) - log2(s1[len - 1] + 5)
  # from 3 to end-2
  pars_center <- sapply(3:(len - 2), function(jj) {
    log2(mean(v1[(jj - 2):(jj + 2)]) + 5) - log2(mean(s1[(jj - 2):(jj + 2)]) + 5)
  })
  pars_base <- c(pars01, pars02, pars_center, pars12, pars11)
  c(S1_V1[ii, 1],mean(pars_base))
  # pars[ii,2] <<- paste(pars_base, collapse = ';')
  # pars[ii,1] <<- S1_V1[ii, 1]
  # pars[ii,2] <<- mean(pars_base)
}

Sys.time()
system.time({
  
pars <- ldply(1:nrow(S1_V1), .parallel = T, .paropts = list(.export=c('s1_data', 'v1_data', 'S1_V1', 's1_depth', 'v1_depth')), .fun = pars_cal )
    
})
pars_GM12892 <- pars
# mappding id

# combine the single together ----
pars <- left_join(pars_GM12878, pars_GM12891, by = 'V1' ) %>% 
  left_join(pars_GM12892, by = 'V1')

l_ply(2:4, function(ii){
  pars[,ii] <<- as.numeric(pars[,ii])
})

pars$mean <- rowMeans(pars[, 2:4]) 
pars_nature <- pars

pars_nature <- pars_nature[,c(1,5)]
colnames(pars_nature) <- c('name','pars_nat')
# mapping id
ensg2refseq <- read_csv('./raw_data/ensg_refseq.csv') %>% distinct()
colnames(ensg2refseq) <- c('ensg', 'enst', 'ref_mRNA', 'ref_ncRNA')

pars1 <- inner_join(pars_nature, ensg2refseq, by = c('name' = 'ref_mRNA')) %>%
  select(ensg, enst, pars_nat)

pars2 <- inner_join(pars_nature, ensg2refseq, by = c('name'= 'ref_ncRNA')) %>% 
  select(ensg, enst, pars_nat)

library(splitstackshape)
pars3 <- filter(pars_nature, grepl('ENST', name) == T) %>% 
  cSplit(1, '.', direction = 'wide') %>% 
  as.data.frame() %>% 
  rename(enst = name_1) %>% 
  select(2,1) %>% 
  inner_join(ensg2refseq, by = c('enst' = 'enst'))%>% 
  select(ensg, enst, pars_nat)
pars_nat <- rbind_all(list(pars1, pars2, pars3))


# pars_db <- src_sqlite('./data/pars.db', create = F)
# copy_to(pars_db, pars_nature, indexes = list('name'))
# copy_to(pars_db, ensg2refseq, indexes = list(c('enst', 'ref_mRNA', 'ref_ncRNA')))
# 
# pars1 <- tbl(pars_db,sql('select  ensg2refseq.enst, pars_nature.pars_nat from pars_nature left join ensg2refseq on pars_nature.name = ensg2refseq.enst '))
save.image()
