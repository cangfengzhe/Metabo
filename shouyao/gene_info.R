gene_info <- read_csv('../../gs/gene_info')
system.time({
gene_info <- read_delim('../../gs/gene_info', delim = '\t', skip = 1)
})
library(readr)

