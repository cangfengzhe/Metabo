drug <- read.csv("D:/deskTop/info_drug.csv", stringsAsFactors = F)
drugNull <- drug[which(is.na(drug[, 5]) == T), ]

drugaa <- drug[which(drug[, 5] != ""), ]
View(drugaa)
preurl <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/"
ii <<- 0
result <- matrix(NA, nrow(drug), 3)
aa <- sapply(1:nrow(drugaa), function(x) {
    kw <- drugaa[x, 5]
    
    url <- paste(c(preurl, kw, "/synonyms/txt"), collapse = "")
    drug_name <- readLines(url)
    drug_name <- unique(drug_name)
    
    result[x, 1] <<- drugaa[x, 2]
    result[x, 2] <<- drugaa[x, 5]
    result[x, 3] <<- paste(drug_name, collapse = ", ")
    print(x)
})

write.csv(result, file = "./data/drug_names.csv")




library(splitstackshape)
drug_name <- cSplit(indt = result, splitCols = 3, sep = "|", direction = "long")
View(drug_name)
drug_name <- as.data.frame(drug_name)
drug_name <- drug_name[, c(1, 3)]
drug_all <- rbind(drug_name, drugNull[, 2:3])
colnames(drug_name) <- c("drug_id", "name")
colnames(drug_all) <- c("drug_id", "drug_name")

library(RMySQL)
conn <- dbConnect(drv = dbDriver("MySQL"), dbname = "cancerdr", 
    host = "localhost", user = "root", password = "")
dbWriteTable(conn, name = "drug_synomyms", value = result)


gene <- read.csv("./data/geneInfo.csv", stringsAsFactors = F)
View(gene)
genedb <- read.csv("D:/deskTop/info_gene.csv", stringsAsFactors = F)
library(sqldf)
names(gene)
names(genedb)
aa <- sqldf("select * from genedb left join gene on genedb.entrez_id=gene.GeneID")
detach(package:RMySQL)
View(aa)
write.csv(aa, file = "aa.csv") 
