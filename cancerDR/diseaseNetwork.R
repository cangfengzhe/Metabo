# disease mutation, methylation, miRNA targets- network
# 获得 mutation, methylation, mirRNA 共有的靶点
#disease
library(RMySQL)
library(sqldf)
conn <- dbConnect(MySQL(),'root', '', dbname='cancerDR')
info_mir <- dbReadTable(conn, 'info_mir')

mir_target <- sqldf('select * from info_mir left join 
                    mirAll on info_mir.miRbase_id=mirAll.mirID')
mir_target <- mir_target[,c('mir_id', 'geneID')]
mir_target <- na.omit(mir_target)
names(mir_target)[2] <- 'gene_id'
dbWriteTable(conn,'mir_target', mir_target)
# 完成此步骤后需要到mysql数据库修改 字段的类型,改为整型

# 得到疾病中mutation, methylation, mirna 3者共有的gene,以及对应的细胞系得到数目
disease_mut <- dbGetQuery(conn, 'select distinct info_disease.disease_id,mutation.cell_id, info_disease.disease_name, 
		mutation.gene_id from  info_disease 
           left join cell_disease on cell_disease.disease_id = info_disease.disease_id
           left join mutation on cell_disease.cell_id=mutation.cell_id
           ')
disease_mut <- na.omit(disease_mut)
disease_mut_group <- sqldf('select disease_id, disease_name, gene_id, count(cell_id) from disease_mut group by disease_id, gene_id')

disease_meth <- dbGetQuery(conn, 'select distinct info_disease.disease_id, info_disease.disease_name, methylation.cell_id,
    methylation.gene_id from info_disease
    left join cell_disease on cell_disease.disease_id = info_disease.disease_id
           left join methylation on cell_disease.cell_id=methylation.cell_id
                           ')
disease_meth <- na.omit(disease_meth)
disease_meth_group <- sqldf('select disease_id, disease_name, gene_id, count(cell_id) from disease_meth group by disease_id, gene_id')

disease_mir <- dbGetQuery(conn, 'select distinct info_disease.disease_id, info_disease.disease_name, mir.cell_id,
    mir_target.gene_id from info_disease left join  cell_disease on cell_disease.disease_id = info_disease.disease_id
                          left join mir on mir.cell_id = cell_disease.disease_id
                          left join mir_target on mir_target.mir_id = mir.mir_id;')
disease_mir <- na.omit(disease_mir)
disease_mir_group <- sqldf('select disease_id, disease_name, gene_id, count(cell_id) from disease_mir group by disease_id, gene_id')

# 将上面三者进行合并
disease_mut_meth <- sqldf('select * from disease_meth_group full join disease_mut_group
      on disease_mut_group.disease_id = disease_meth_group.disease_id
      and disease_mut_group.gene_id = disease_meth_group.gene_id')




