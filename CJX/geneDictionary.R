humanGene <- read.csv("D:\\deskTop\\LPD\\cjx\\HumanGeneDictionary.csv", stringsAsFactors = F)
View(humanGene)
splitTab <- function(input) {
    split.str <- strsplit(input)
    
}

geneDic <- matrix(NA, 2e+06, 2)
kk <- 0
for (ii in 1:nrow(humanGene)) {
    
    
    
    gene0 <- strsplit(humanGene[ii, 4], "\\|")[[1]]
    gene1 <- strsplit(humanGene[ii, 9], "\\|")[[1]]
    gene <- c(humanGene[ii, 3], humanGene[ii, 5], humanGene[ii, 7], humanGene[ii, 
        8], gene0, gene1)
    gene <- unique(gene)
    len <- length(gene)
    geneDic[(kk + 1):(kk + len), 1] <- humanGene[ii, 2]
    geneDic[(kk + 1):(kk + len), 2] <- gene
    print(ii)
    kk <- kk + len
}
geneDic <- geneDic_1[geneDic_1[, 2] != "-", ]
aa <- geneDic[, 2] == "-"
aa <- as.numeric(aa)
sum(aa)
write.csv(geneDic, "HumanGeneDictionary.csv")
geneDic_1 <- na.omit(geneDic)
nrow(geneDic_1) 
