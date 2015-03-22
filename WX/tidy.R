# read the data
gene_degree_utr3 <- read.csv('/Users/lipidong/work/WX/miRNA_network_analysis/use/gene_indegree_3pUTR.csv', stringsAsFactors = F)
gene_degree_utr3$ensembl <- do.call('rbind', strsplit(gene_degree_utr3[,1], split = '\\.'))[,1]
gene_degree_utr3 <- gene_degree_utr3[,c(3,1,2)]


mircode <- read.delim('/Users/lipidong/work/WX/mircode_highconsfamilies.txt', stringsAsFactors = F)
ensembl <- strsplit(mircode[,1], '\\.')
mircode$ensembl <- do.call('rbind',ensembl)[,1]
mircode <- mircode[,c(13,4)]

mRNA_hl <- read.csv('/Users/lipidong/work/WX/human\ mRNA\ half\ life\ data.csv', stringsAsFactors = F)[,c(1,3,4)]

ensembl2genbank <- read.csv('/Users/lipidong/work/WX/ensembl_protein_gene.csv', stringsAsFactors = F)
colnames(ensembl2genbank) <- c('entrez','ensembl_pro', 'ensembl_gene', 'genbank')

pro_hl <- read.csv('/Users/lipidong/work/WX/protein_hl.csv', stringsAsFactors = F)


pro_abun <- read.table('~/work/WX/protein_abundance.txt', stringsAsFactors = F, header = T)
colnames(pro_abun) <- c('id', 'ensembl_pro', 'pro_abundance')
tmp <- do.call('rbind',strsplit(pro_abun[,2], '\\.'))[,2]
pro_abun[,2] <- tmp
pro_abun <- pro_abun[,2:3]

library(sqldf)
all_data <- sqldf('select distinct gene_degree_utr3.*, mircode.*, mRNA_hl.*, pro_abun.* from gene_degree_utr3 
                  left join ensembl2genbank on gene_degree_utr3.ensembl = ensembl2genbank.ensembl_gene
                  left join mRNA_hl on mRNA_hl.Accession = ensembl2genbank.genbank
                  left join pro_abun on pro_abun.ensembl_pro= ensembl2genbank.ensembl_pro
                  left join  mircode on mircode.ensembl=gene_degree_utr3.ensembl')
save.image('./data/tidy.rdata')
