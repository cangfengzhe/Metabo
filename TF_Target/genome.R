# read the yeast Genome
library(seqinr)
# read the sequence of every chromosome
files <- dir('./raw_data/chrome/')
sapply(files, function(x){
  path <- paste(c('./raw_data/chrome/', x), collapse = '')
  name <- strsplit(x, '\\.')[[1]][1]
  assign(name, unlist(read.fasta(path, as.string = T)), envir = .GlobalEnv)
  x
})
# 根据chromosome，起始位点，终止位点，提取序列
extract_seq <- function(tran_start, tran_end, num){
  chrome <- paste(c('chrome_', num), collapse = '')
  # seq <- get(chrome, envir = .GlobalEnv)
  str <- paste(c('seq <- chrome_', num), collapse = '')
  eval(parse(text = str))
  substr(seq, tran_start, tran_end)
}



