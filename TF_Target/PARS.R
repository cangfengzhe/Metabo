# calulate mRNA PARS data
library(dplyr)
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
