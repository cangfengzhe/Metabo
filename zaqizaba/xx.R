# date:2015-10-10
# author: Li Pidong
# 查找804个分子-症状 对应的杂志名称，并对应影响因子
setwd('./tcm_xx')
library(RMySQL)
options(stringsAsFactors =FALSE)
con = dbConnect(drv=RMySQL::MySQL(), user='root', password='', dbname='tcmsp')
# 导出 分子信息
info_mol <- dbReadTable(con, 'LSP_TCMSP_InfoMolecule')

# mol804
mol804 <- read_csv('./804_mol.csv')
colnames(mol804)[2] <- 'name'
drug_symptom_count <- as.data.frame(drug_symptom_count)
drug_symptom_count$V1 <- as.character(drug_symptom_count$V1)
to_lower <- function(x){
  tryCatch({
    return(tolower(x))
  }, error=function(e){
    return(x)
  })
}
drug_symptom_count$V1 <- sapply(drug_symptom_count$V1, to_lower)

# 处理希腊字母乱码
drug_symptom_count$V1[which(drug_symptom_count$V1==drug_symptom_count$V1[3840])] <- 'delta-tocopherol'

info_mol$molecule_name <- sapply(info_mol$molecule_name, to_lower)
info_mol$molecule_name[which(info_mol$MOL_ID=='MOL008695')] <- 'delta-tocopherol'

mol_sym <- mol804 %>% 
  left_join(info_mol, by = c('ID'='MOL_ID')) %>% 
  select(ID, name, svddPredict, molecule_name) %>% 
  left_join(drug_symptom_count, by = c('molecule_name' = 'V1'))

molSym$molName <- sapply(molSym$molName, to_lower)
mol_sym_pid <- mol804 %>% 
  left_join(info_mol, by = c('ID'='MOL_ID')) %>% 
  select(ID, name, svddPredict, molecule_name) %>% 
  left_join(molSym, by = c('molecule_name' = 'molName')) %>% 
  left_join(pubmed_abstract, by = 'pmid') 
colnames(mol_sym_pid)

mol_sym_pid <- select(mol_sym_pid, c(1:6, 10))
journal_name <- mol_sym_pid$journal %>% unique() %>% data.frame(name = .)

# journal info ------
info_journal <- read_csv('./journal_if.csv') %>% 
  select(c(1:4, 17))
colnames(info_journal)[3] <- 'abbr'
info_journal$abbr <- sapply(info_journal$abbr, to_lower)
journal_name$name <- sapply(journal_name$name, to_lower)
journal_name_if <- journal_name %>% left_join(info_journal, by = c('name' = 'abbr'))


# joural info xiaobai------
info_journal_xb <- read_csv('jcr_impact factor.csv')
info_journal_xb$jiancheng <- sapply(info_journal_xb$jiancheng, to_lower)
journal_name_if_xb <- journal_name %>% left_join(info_journal_xb, by = c('name' = 'jiancheng'))
ß
journal_name_if_xb2 <- sqldf('select * from journal_name left join info_journal_xb on journal_name.name = info_journal_xb.jiancheng or journal_name.name = info_journal_xb.JOURNAL')

# pubmed journal name matching
 mol_sym_pid %>% left_join(pubmed_jcr, by = c(''))
 sqldf('drop table pubmed_jcr')
 journal_issn <-sqldf('select * from journal_name left join pubmed_jcr on journal_name.name = pubmed_jcr.JournalTitle or journal_name.name= pubmed_jcr.IsoAbbr or journal_name.name= pubmed_jcr.MedAbbr')
journal_issn <- journal_issn[, c(1, 5, 6)]
colnames(journal_issn) <- c('name', 'issn_print', 'issn_online')
colnames(info_journal)[5] <- 'if_avg5'
colnames(info_journal)[4] <- 'title'

journal_if <- sqldf('select distinct journal_issn.name, info_journal.if_avg5 from journal_issn left join info_journal on journal_issn.issn_print = info_journal.ISSN or journal_issn.issn_online = info_journal.ISSN or journal_issn.name=info_journal.title or info_journal.abbr=journal_issn.name')
journal_if <- na.omit(journal_if)
mol_sym_pid$journal <- to_lower(mol_sym_pid$journal)
pubmed_if <- mol_sym_pid %>% left_join(journal_if, by = c('journal' = 'name'))

pubmed_if <- na.omit(pubmed_if)
length(unique(pubmed_if$name)) # 760
filter(pubmed_if, if_avg5>2) %>% select(pmid) %>% distinct() %>% nrow
filter(pubmed_if, if_avg5>2) %>% select(pmid) %>% distinct() %>% nrow
length(mol_sym_pid$pmid %>% unique)
pubmed_sym_if <- filter(pubmed_if, if_avg5>2) %>% group_by(name, symptom) %>% summarize(count = n())

write_csv(pubmed_sym_if, path = './pubmed_sym_count.csv')
