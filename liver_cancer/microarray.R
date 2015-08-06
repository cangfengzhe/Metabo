library(GEOquery)

## Array quality in mice studies was assessed with the arrayQualityMetrics package of Bioconductor (http://www.bioconductor.org). Data were normalized and logged-2 transformed using the RMA algorithm.

GSE63898 <- getGEO(filename = '/Users/lipidong/Downloads/liver_cancer/GSE63898_family.soft')

# dir.create('./data')
# save.image('./data/data.rdata')
load('./data/data.rdata')
gsmlist <- GSMList(GSE63898)
rm(GSE63898)
gc()
probe_ids <- as.vector(Table(gsmlist[[1]])$ID_REF)
sample_names <- sapply(1:length(gsmlist), function(ii){
  names(gsmlist[ii])
})
matrix_data <- ldply(1: length(gsmlist), .progress = 'text', .fun = function(ii){
  tab <- Table(gsmlist[[ii]])
  id_match <- match(tab$ID_REF, probe_ids)
  tab$VALUE[id_match] %>% as.numeric()
})

matrix_data <- t(matrix_data)
colnames(matrix_data) <- sample_names
rownames(matrix_data) <- probe_ids
# expriment design ----
experTable <- ldply(1: length(gsmlist), function(ii){
 c(names(gsmlist[ii]), gsmlist[[ii]]@header$source_name_ch1)
})
# mapping gene ----
matrix_data_df <- matrix_data %>% as.data.frame()
matrix_data_df$probe_id <- rownames(matrix_data)
colnames(matrix_data_df)
matrix_data_entrez <- matrix_data_df %>% left_join(gpl1, by = 'probe_id')
colnames(matrix_data_entrez)
express_data <- matrix_data_entrez %>% select(c(399, 1: 396))
type_index <- match(colnames(express_data)[2: 397], experTable[,1])
type <- experTable[type_index, 2]
dim(express_data)
express_data <- filter(express_data, gene_id != '---')
#express_data1 <- rbind(c(NA, type), express_data)

# export matrix data ----
#write.csv(express_data1, file = 'ganaiganyinghua.csv')
experTable$index = NA
experTable[which(experTable$V2 == 'hepatocellular carcinoma'), 3] <- 1
experTable[which(experTable$V2 != 'hepatocellular carcinoma'), 3] <- 0
express_data_order <- express_data[c(experTable[experTable$V2 == 'hepatocellular carcinoma', 1], experTable[experTable$V2 != 'hepatocellular carcinoma', 1]), ]

# 该数据已经使用RMA函数，进行了标准化和 log2 转换，所以我们可以直接进行差异表达分析


library(affy)

boxplot(matrix_data[, c(1:50)], boxwex=0.6, notch=T, outline=T, las=2)
library(limma)
save.image('./data/data.rdata')

experTable[,2] <- experTable[,2] %>% as.factor()
design <- model.matrix(~ V2+0, experTable)

colnames(design) <- c("cirrhosis", "carcinoma")
fit <- lmFit(matrix_data, design)
contrast.matrix <- makeContrasts(cirrhosis-carcinoma,levels=design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2, 0.01)
tT <- topTable(fit2, adjust="fdr", sort.by="B", number=250000)
View(tT)

cirrhosis <- filter(experTable, V2 == 'cirrhosis') %>% select(V1) %>% as.data.frame()
cancer <- filter(experTable, V2 != 'cirrhosis') %>% select(V1) %>% as.data.frame()
GPLList()
