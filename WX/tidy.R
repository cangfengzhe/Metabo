# read the data
gene_degree <- read.csv('/Users/lipidong/work/WX/miRNA_network_analysis/use/gene_indegree_3pUTR.csv', stringsAsFactors = F)

gene_degree <- read.csv('/Users/lipidong/work/WX/miRNA_network_analysis/use/gene_indegree_5pUTR.csv', stringsAsFactors = F)

gene_degree <- read.csv('/Users/lipidong/work/WX/miRNA_network_analysis/use/gene_indegree_CDS.csv', stringsAsFactors = F)
gene_degree <- read.csv('/Users/lipidong/work/WX/miRNA_network_analysis/use/gene_indegree_Intergenic_ncRNA.csv', stringsAsFactors = F)


gene_degree$ensembl <- do.call('rbind', strsplit(gene_degree[,1], split = '\\.'))[,1]
gene_degree <- gene_degree[,c(3,1,2)]


mircode <- read.delim('/Users/lipidong/work/WX/mircode_highconsfamilies.txt', stringsAsFactors = F)
ensembl <- strsplit(mircode[,1], '\\.')
mircode$ensembl <- do.call('rbind',ensembl)[,1]
mircode <- mircode[,c(13,4)]

ensembl2genbank <- read.csv('/Users/lipidong/work/WX/ensembl_protein_gene.csv', stringsAsFactors = F)
colnames(ensembl2genbank) <- c('entrez','ensembl_pro', 'ensembl_gene', 'genbank')

mRNA_hl <- read.csv('/Users/lipidong/work/WX/human\ mRNA\ half\ life\ data.csv', stringsAsFactors = F)[,c(1,3,4)]

mRNA_hl <- sqldf('select ensembl2genbank.entrez, avg(mRNA_hl.Rate) as mRNA_halflife from mRNA_hl left join ensembl2genbank on ensembl2genbank.genbank = mRNA_hl.Accession group by ensembl2genbank.entrez')
mRNA_hl <- na.omit(mRNA_hl)
# removed
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
ensembl <- unique(ensembl2genbank[, 1:3])

pro_abun_tmp <- sqldf('select distinct ensembl.ensembl_gene, avg(pro_abun.pro_abundance) as pro_abundance from pro_abun left join ensembl on ensembl.ensembl_pro = pro_abun.ensembl_pro group by ensembl.ensembl_gene')
pro_abun <- na.omit(pro_abun_tmp)



NR_hl <- read.csv('/Users/lipidong/work/WX/NR_hl.csv', stringsAsFactors = F)
NR_hl <- left_join(NR_hl, ref2entrez, by = c('name'= 'accession'))
NR_hl_tmp <- sqldf('select  gene_id,avg(RPKM) as RPKM, avg(hl) as hl from NR_hl group by gene_id')
NR_hl_tmp[NR_hl_tmp$hl ==0,3] <- NA
NR_hl <- NR_hl_tmp

genbank_hl <- read.csv('/Users/lipidong/work/WX/genbank_hl.csv', stringsAsFactors = F)
genbank_hl <- left_join(genbank_hl, ensembl2genbank, by=c('name'='genbank'))
genbank_hl <- genbank_hl[, c(7,1,3,4)]
genbank_hl_tmp <- sqldf('select ensembl_gene, avg(RPKM) as RPKM, avg(hl) as hl from genbank_hl group by ensembl_gene')
genbank_hl_tmp[genbank_hl_tmp[,3]==0, 3] <- NA
genbank_hl <- genbank_hl_tmp

# mRNA abundance ----
mRNA_abun <- read.csv('/Users/lipidong/work/WX/rna_abundance.csv', stringsAsFactors = F)
tissue <- data.frame(tissue=unique(mRNA_abun[,2])[45:76])
mRNA_abun_tissue <- sqldf('select mRNA_abun.* from mRNA_abun inner join tissue on tissue.tissue=mRNA_abun.Sample')
mRNA_abun_tissue <- sqldf('select distinct Gene,avg(value) as avg from mRNA_abun_tissue group by Gene')

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
PARS$name2 <- do.call('rbind', strsplit(PARS[,1], split = '\\.'))[,1]
ensg_enst_entrez <- read.csv('/Users/lipidong/work/protein\ bundunce/data/ENSG_ENST_Entrez.csv', stringsAsFactors = F)
id_map <- sqldf('select * from ')
pars <- sqldf('select * from PARS left join ensg_enst_entrez on PARS.name2 = ensg_enst_entrez.enst')
pars <- sqldf('select * from pars left join ref2entrez on ref2entrez.accession = pars.name2 
              left join ensg_enst_entrez on ensg_enst_entrez.entrez = ref2entrez.gene_id')
pars01 <- pars[,c(-6,-7,-9,-11)]
pars01[grep('ENSG', pars01[,7]),5] <- pars01[grep('ENSG', pars01[,7]),7] 
pars02 <- unique(pars01[, c(4,5,2,3)])

pars_tmp <- sqldf('select ensg, avg(all_score_mean) as PARS, avg(part_score_mean) as PARS01 from pars02 group by ensg')
pars <- na.omit(pars_tmp)


library(sqldf)
library(RSQLite)


# revise
aa <- left_join(gene_degree, ensembl2genbank, by = c('ensembl' = 'ensembl_gene'))

all_data <- sqldf('select  distinct ensembl2genbank.entrez, gene_degree.*, mRNA_hl.mRNA_halflife, pro_abun.pro_abundance, mRNA_abun_tissue.avg as mRNA_abundance,pars.PARS from gene_degree 
                  left join ensembl2genbank on gene_degree.ensembl = ensembl2genbank.ensembl_gene
                  left join mRNA_hl on mRNA_hl.entrez = ensembl2genbank.entrez
                  left join pro_abun on pro_abun.ensembl_gene= gene_degree.ensembl
                  left join mRNA_abun_tissue on mRNA_abun_tissue.Gene = gene_degree.ensembl
                  left join pars on pars.ensg = gene_degree.ensembl')

all_data_1 <- sqldf('select all_data.*, NR_hl.RPKM as NR_RPKM, NR_hl.hl as NR_halflife, genbank_hl.RPKM as genbank_RPKM, genbank_hl.hl as genbank_halflife from all_data left join NR_hl on all_data.entrez = NR_hl.gene_id left join genbank_hl on genbank_hl.ensembl_gene = all_data.ensembl')

save.image('./data/tidy.rdata')
write.csv(all_data, file = './data/Intergenic_ncRNA.csv')




