# process the pars of yeast

V1 <- read.delim('./raw_data/PARS/sce_V1.tab', stringsAsFactors = F, header = F)
S1 <- read.delim('./raw_data/PARS/sce_S1.tab', stringsAsFactors = F, header = F)
score <- read.delim('./raw_data/PARS/sce_score.tab', stringsAsFactors = F, header = F)

# split the read cout by ';'
s1_data <- strsplit(S1[, 3], ";")
v1_data <- strsplit(V1[, 3], ";")
score_data <- strsplit(score[,3], ';')
# calulate the sequence depth
s1_tmp <- 0
  sapply(1: length(s1_data), function(ii){
    s1_tmp <<- s1_tmp+ sum(as.numeric(s1_data[[ii]]))
  })
s1_depth <- s1_tmp/sum(S1[,2])


v1_tmp <- 0
sapply(1: length(v1_data), function(ii){
  v1_tmp <<- v1_tmp+ sum(as.numeric(v1_data[[ii]]))
})


v1_depth <- v1_tmp/sum(V1[,2])

S1_V1 <- sqldf("select * from S1 inner join V1 on S1.V1=V1.V1")
S1_V1 <- S1_V1[, c(1, 3, 6)]
colnames(S1_V1) <- c("name", "S1", "V1")
s1_data <- strsplit(S1_V1[, 2], ";")
v1_data <- strsplit(S1_V1[, 3], ";")
pars = data.frame()  # save the mean
score2 <- sapply(1:nrow(S1_V1), function(ii) {
  
  s1 <- as.numeric(s1_data[[ii]])#/s1_depth
  v1 <- as.numeric(v1_data[[ii]])#/v1_depth
   log(v1/s1) 
})
 
score %>% View()
 
score_num <- score_data[[1]] %>% as.numeric() 
score1_num <- score1[[1]] %>% na.omit()
aa <- data.frame(raw = score_num, cal = score1[[1]], cal_nonorm = score2[[1]])

View(aa)
aa <- filter(aa, is.infinite(cal) == F ) %>% na.omit()
