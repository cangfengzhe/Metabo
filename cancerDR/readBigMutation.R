library(ff)
system.time({
  
mutInfo <- read.delim.ffdf(file='D:/deskTop/CosmicWGS_MutantExport.csv/CosmicWGS_MutantExport.tsv')
})
system.time({
  
mutInfo <- read.delim(file='D:/deskTop/CosmicWGS_MutantExport.csv/CosmicWGS_MutantExport.tsv')
})

mut <- mutInfo[,c(1,13,14,15,16,18)]
mutation <- read.csv('./data/procMutation.csv', header = T)

library(sqldf)
result <- sqldf('select * from mutation left join mut on 
                mutation.gene=mut.gene AND mutation.type=mut.mutation_aa')



colnames(mut)[1] <- 'gene'

colnames(mut)[4] <- 'mutation_aa'

filter(tbl_df(mut), grepl('p\\.K164fs.*', mutation_aa, perl = T) ) %>% View()

mut2 <- read.delim('D:/deskTop/CosmicMutantExportCensus.tsv/CosmicMutantExportCensus.tsv')
View(mut2)
mut2 <- mut2[,c(1,13,14,15,16,18)]
colnames(mut2)[1] <- 'gene'

colnames(mut2)[4] <- 'mutation_aa'
result2 <- sqldf('select * from mutation left join mut on 
                mutation.gene=mut.gene AND mutation.type=mut.mutation_aa')

mutation2 <- read.csv('./data/mutation2.csv')
result2 <- sqldf('select * from mutation2 left join mut2 on 
                mutation2.gene=mut2.gene AND mutation2.type=mut2.mutation_aa')

