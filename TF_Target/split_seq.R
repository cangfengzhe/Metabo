# split cds utr in order to calulate the mF
library(Biostrings)
pos <- read.table("/Users/lipidong/work/protein bundunce/data/sce_gene_position .txt", 
    stringsAsFactors = F)
cds_pos <- filter(pos, V2 == "CDS")
utr5_pos <- filter(pos, V2 == "5UTR")
utr3_pos <- filter(pos, V2 == "3UTR")
  
dna_seq <- readDNAStringSet("/Users/lipidong/work/protein bundunce/data/sce_gene_cds_utr.fasta")
# 下载的pos表中有，但是序列里没有的数据
dna_seq_add <- readDNAStringSet("./data/unknow.fasta")

dna_seq <- rbind(as.data.frame(dna_seq, stringsAsFactors = F), 
    as.data.frame(dna_seq_add, stringsAsFactors = F))
dna_seq[, 1] <- as.character(dna_seq[, 1])

sapply(1:nrow(cds_pos), function(ii) {
    cds_pos[ii, 5] <<- substr(dna_seq[cds_pos[ii, 1], 1], cds_pos[ii, 
        3], cds_pos[ii, 4])
    print(ii)
})
# filter(cds_pos, is.na(V5) == T)

# 3'UTR
sapply(1:nrow(utr3_pos), function(ii) {
  #序列加载到对应的名字和位置上
    utr3_pos[ii, 5] <<- substr(dna_seq[utr3_pos[ii, 1], 1], utr3_pos[ii, 
        3], utr3_pos[ii, 4])
    print(ii)
})
filter(utr3_pos, is.na(V5) == T)

# 5'UTR
sapply(1:nrow(utr5_pos), function(ii) {
    utr5_pos[ii, 5] <<- substr(dna_seq[utr5_pos[ii, 1], 1], utr5_pos[ii, 
        3], utr5_pos[ii, 4])
    print(ii)
})
filter(utr5_pos, is.na(V5) == T)


unknow <- unique(c(filter(cds_pos, is.na(V5) == T)[, 1], filter(utr3_pos, 
    is.na(V5) == T)[, 1], filter(utr5_pos, is.na(V5) == T)[, 1]))



# download the unknow fasta
preurl <- "http://genie.weizmann.ac.il/pubs/PARS10/data/gene_pages/fasta/"

sapply(1:length(unknow), function(ii) {
    url <- paste(c(preurl, unknow[ii], ".fa"), collapse = "")
    fasta <- readLines(url)
    write.table(fasta, file = "./data/unknow.fasta", append = T, 
        quote = F, col.names = F, row.names = F)
    print(ii)
})

# 下载未知的序列后， 返回执行上面的步骤




# 导出数据
write.csv(cds_pos, "./data/cds_pos.csv")
write.csv(utr3_pos, "./data/utr3_pos.csv")
write.csv(utr5_pos, "./data/utr5_pos.csv") 


# dn_ds

View(da_ds)

