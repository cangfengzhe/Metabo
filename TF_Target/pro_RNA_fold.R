out <- read_lines('./data/fasta/out.txt')
out2 <- out[(1:(length(out)/3))*3]
mf <- gregexpr('-[0-9]+\\.[0-9]+', out2) %>% 
regmatches(out2, .)

out_name <- out[((1:(length(out)/3))-1)*3+1]
name <- gregexpr('(?<=>).*', out_name, perl = T) %>% 
  regmatches(out_name, .)

result <- data.frame(name = name %>% unlist(), mf = mf %>% unlist())
library(splitstackshape)
library(reshape2)
result_1 <- cSplit(result, 1, sep = '_', direction = 'wide')
colnames(result_1) <- c('mF', 'name', 'type', 'length')
result_1 <- as.data.frame(result_1) %>% select(c(3,2,4,1))
result <- dcast(result_1, name~type, value.var = 'mF')
result_len <- dcast(result_1, name~type, value.var = 'length')
write.csv(result_len, file = './data/fasta/gene_length.csv')

result_mf <- dcast(result_1, name~type, value.var = 'mF')
write.csv(result_mf, file = './data/fasta/gene_mf.csv')




