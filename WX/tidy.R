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
pro_hl <- pro_hl[,c(1,4,5)]
colnames(pro_hl) <- c('ipi', 'pro_turnover', 'pro_whole_half_life')
ipi2ensembl <- read.csv('/Users/lipidong/work/WX/ipi2ensembl.csv', stringsAsFactors = F, header = F)
ipi2ensembl <- ipi2ensembl[,c(5,2,3)]
colnames(ipi2ensembl) <- c('ipi', 'ensembl_pro', 'ensembl_gene')


pro_abun <- read.table('~/work/WX/protein_abundance.txt', stringsAsFactors = F, header = T)
colnames(pro_abun) <- c('id', 'ensembl_pro', 'pro_abundance')
tmp <- do.call('rbind',strsplit(pro_abun[,2], '\\.'))[,2]
pro_abun[,2] <- tmp
pro_abun <- pro_abun[,2:3]

NR_hl <- read.csv('/Users/lipidong/work/WX/NR_hl.csv', stringsAsFactors = F)
genbank_hl <- read.csv('/Users/lipidong/work/WX/genbank_hl.csv', stringsAsFactors = F)

# mRNA abundance ----
mRNA_abun <- read.csv('/Users/lipidong/work/WX/rna_abundance.csv', stringsAsFactors = F)
tissue <- data.frame(tissue=unique(mRNA_abun[,2])[45:76])
mRNA_abun_tissue <- sqldf('select mRNA_abun.* from mRNA_abun inner join tissue on tissue.tissue=mRNA_abun.Sample')

# PARS score ----
PARS01 <- read.csv('/Users/lipidong/work/WX/pars_GM12892_MBE.csv', stringsAsFactors = F)
PARS02 <- read.csv('/Users/lipidong/work/WX/pars_GM12891_MBE.csv', stringsAsFactors = F)
PARS03 <- read.csv('/Users/lipidong/work/WX/pars_GM12878_MBE.csv', stringsAsFactors = F)
PARS01 <- PARS01[,c(2,5,6)]
PARS02 <- PARS02[,c(2,5,6)]
PARS03 <- PARS03[,c(2,5,6)]
colnames(PARS01) <- c('name', 'all_score', 'part_score')
colnames(PARS02) <- c('name', 'all_score', 'part_score')
colnames(PARS03) <- c('name', 'all_score', 'part_score')

PARS <- sqldf('select PARS01.*, PARS02.all_score, PARS02.part_score, PARS03.all_score, PARS03.part_score from PARS01 left join PARS02 on PARS01.name=PARS02.name left join PARS03 on PARS03.name=PARS01.name')
PARS$all_score_mean <- rowMeans(PARS[,c(2,4,6)])
PARS$part_score_mean <- rowMeans(PARS[,c(3,5,7)])
PARS <- PARS[,c(1,8,9)]
PARS_id <- sqldf('select ref2entrez.gene_id,PARS.name, PARS.all_score_mean, PARS.part_score_mean from PARS left join ref2entrez on PARS.name = ref2entrez.accession')
library(sqldf)
library(RSQLite)

all_data <- sqldf('select  distinct ensembl2genbank.entrez, gene_degree_utr3.*, mRNA_hl.accession, mRNA_hl.Rate as mRNA_halflife, pro_abun.pro_abundance, pro_hl.ipi, pro_hl.pro_turnover, mRNA_abun_tissue.Sample, mRNA_abun_tissue.Value as mRNA_abundance from gene_degree_utr3 
                  left join ensembl2genbank on gene_degree_utr3.ensembl = ensembl2genbank.ensembl_gene
                  left join mRNA_hl on mRNA_hl.Accession = ensembl2genbank.genbank
                  left join pro_abun on pro_abun.ensembl_pro= ensembl2genbank.ensembl_pro
                  
                  left join ipi2ensembl on ipi2ensembl.ensembl_gene = ensembl2genbank.ensembl_gene
                  left join pro_hl on pro_hl.ipi = ipi2ensembl.ipi
                  left join mRNA_abun_tissue on mRNA_abun_tissue.Gene = gene_degree_utr3.ensembl')

all_data2 <- sqldf('select distinct * from all_data 
 
left join ref2entrez on ref2entrez.gene_id = all_data.entrez
                   left join NR_hl on NR_hl.name=ref2entrez.accession')

all_data2 <- all_data2[,c(-2, -7, -12, -17)]
all_data2 <- all_data2[,c(-15)]
write.csv(all_data2, file = './data/utr3_mapping.csv')
save.image('./data/tidy.rdata')
