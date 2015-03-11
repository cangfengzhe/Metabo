# process the transcript factor-target data from YEAST database

library(reshape2)
library(dplyr)
raw_data <- read.csv2("/Users/lipidong/work/protein bundunce/data/RegulationMatrix_Documented_2013927.csv", 
    sep = ";", stringsAsFactors = F)
melt_data <- melt(raw_data)
proc_data <- filter(melt_data, value == 1)
write.csv(proc_data, file = "./data/TF_target.csv")


# gene name
gene_name <- read.delim("/Users/lipidong/work/protein bundunce/data/SGD_ORF_name.txt", 
    stringsAsFactors = F, header = F) 
