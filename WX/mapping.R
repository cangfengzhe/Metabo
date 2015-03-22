library(org.Hs.eg.db)
library(sqldf)
library(dplyr)
mircode <- read.delim('/Users/lipidong/work/WX/mircode_highconsfamilies.txt', stringsAsFactors = F)
ensembl <- strsplit(mircode[,1], '\\.')
mircode$ensembl <- do.call('rbind',ensembl)[,1]
mircode <- mircode[,c(13,1:12)]

mRNA_hl <- read.csv('/Users/lipidong/work/WX/human\ mRNA\ half\ life\ data.csv', stringsAsFactors = F)
# genbank2entrez <- as.data.frame(org.Hs.egACCNUM2EG)
# ensembl2entrez <- as.data.frame(org.Hs.egENSEMBL)
# mircode <- sqldf('select ensembl2entrez.gene_id, mircode.* from mircode left join ensembl2entrez on ensembl2entrez.ensembl_id = mircode.ensembl')
# colnames(mircode)[1] <- 'entrez_id'

# mRNA_hl <- sqldf('select genbank2entrez.gene_id, mRNA_hl.* from mRNA_hl left join genbank2entrez on mRNA_hl.accession= genbank2entrez.accession')

ensembl2genbank <- read.csv('/Users/lipidong/work/WX/ensemble2genbank.txt', stringsAsFactors = F)
colnames(ensembl2genbank) <- c('ensembl_id', 'entrez_id', 'genbank_acc')
mRNA2ensembl <- sqldf('select * from mRNA_hl left join ensembl2genbank on mRNA_hl.Accession = ensembl2genbank.genbank_acc')

all_data <- sqldf('select mircode.*, mRNA2ensembl.* from  mircode left join mRNA2ensembl on  mRNA2ensembl.ensembl_id = mircode.ensembl  ')
# union select mircode.*, mRNA2ensembl.* from mRNA2ensembl  left join mircode  on  mRNA2ensembl.ensembl_id = mircode.ensembl
all_data <- unique(all_data)
write.csv(all_data, file = './data/gene_mapping.csv')
names(mircode)
