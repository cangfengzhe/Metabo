# calulate mRNA PARS data
raw_pars <- read.table("/Users/lipidong/work/protein bundunce/data/PARS.tab", 
    stringsAsFactors = F)
data_split <- strsplit(raw_pars[, 3], ";")
sapply(1:nrow(raw_pars), function(ii) {
    raw_pars[ii, 4] <<- mean(as.integer(data_split[[ii]]))
    
})
View(raw_pars)
pars <- raw_pars
save(pars, "pars.dat")
write.csv(pars, file = "pars.csv") 


# calculate the pars score of cds, 5'UTR, 3'UTR respectively
# split the sequence
pos <- read.table("/Users/lipidong/work/protein bundunce/data/sce_gene_position .txt", 
                  stringsAsFactors = F)
cds_pos <- filter(pos, V2 == "CDS")
utr5_pos <- filter(pos, V2 == "5UTR")
utr3_pos <- filter(pos, V2 == "3UTR")
raw_pars <- read.table("/Users/lipidong/work/protein bundunce/data/PARS.tab", 
                       stringsAsFactors = F)

data_split <- strsplit(raw_pars[, 3], ";")
pars01 <- left_join(raw_pars, utr5_pos, by = 'V1') %>% 
              left_join(utr3_pos, by = 'V1') %>% 
                left_join(cds_pos, by = 'V1')

colnames(pars01) <- c('name','len', 'pars','utr5','start5','end5', 'utr3', 'start3', 'end3', 'cds', 'start','end')

proc_pars <- data.frame(name = pars01$name,pars=NA,utr5 =NA,utr3=NA, cds = NA, stringsAsFactors = F) 
sapply(1:12, function(ii){
  pars01[which(is.na(pars01[,ii])),ii] <- 0
  
})

sapply(1:nrow(raw_pars), function(ii) {
  proc_pars[ii, 2] <<- mean(as.integer(data_split[[ii]]))
  proc_pars[ii, 3] <<- mean(as.integer(data_split[[ii]])[pars01[ii,5]:pars01[ii,6]])
  proc_pars[ii, 4] <<- mean(as.integer(data_split[[ii]])[pars01[ii,8]:pars01[ii,9]])
  proc_pars[ii, 5] <<- mean(as.integer(data_split[[ii]])[pars01[ii,11]:pars01[ii,12]])
})
save(pars, "pars.dat")
write.csv(pars, file = "pars.csv") 

