# Version info: R 2.14.1, Biobase 2.15.3, GEOquery 2.23.2,
# limma 3.10.1 R scripts generated Sun Jan 11 21:55:51 EST 2015

################################################################ Differential expression analysis with limma
library(Biobase)
library(GEOquery)
library(limma)

# load series and platform data from GEO

gset <- getGEO("GSE43494", filename = "./Raw_Data/GSE43494-GPL14613_series_matrix.txt.gz", 
    GSEMatrix = TRUE)
if (length(gset) > 1) idx <- grep("GPL14613", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# make proper column names to match toptable
fvarLabels(gset) <- make.names(fvarLabels(gset))

# group names for all samples
sml <- c("G0", "G0", "G0", "G1", "G1", "G1")

# log2 transform
ex <- exprs(gset)
qx <- as.numeric(quantile(ex, c(0, 0.25, 0.5, 0.75, 0.99, 1), na.rm = T))
LogC <- (qx[5] > 100) || (qx[6] - qx[1] > 50 && qx[2] > 0) || (qx[2] > 
    0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
if (LogC) {
    ex[which(ex <= 0)] <- NaN
    exprs(gset) <- log2(ex)
}

# set up the data and proceed with analysis
fl <- as.factor(sml)
gset$description <- fl
design <- model.matrix(~description + 0, gset)
colnames(design) <- levels(fl)
fit <- lmFit(gset, design)
cont.matrix <- makeContrasts(G1 - G0, levels = design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2, 0.01)
tT <- topTable(fit2, adjust = "fdr", sort.by = "B", number = 250)

tT <- subset(tT, select = c("ID", "adj.P.Val", "P.Value", "t", 
    "B", "logFC", "miRNA_ID_LIST", "SPOT_ID"))
write.table(tT, file = stdout(), row.names = F, sep = "\t")

################################################################ Boxplot for selected GEO samples
library(Biobase)
library(GEOquery)

# load series and platform data from GEO

gset <- getGEO("GSE43494", GSEMatrix = TRUE)
if (length(gset) > 1) idx <- grep("GPL14613", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# group names for all samples in a series
sml <- c("G0", "G0", "G0", "G1", "G1", "G1")

# order samples by group
ex <- exprs(gset)[, order(sml)]
sml <- sml[order(sml)]
fl <- as.factor(sml)
labels <- c("P", "C")

# set parameters and draw the plot
palette(c("#dfeaf4", "#f4dfdf", "#AABBCC"))
dev.new(width = 4 + dim(gset)[[2]]/5, height = 6)
par(mar = c(2 + round(max(nchar(sampleNames(gset)))/2), 4, 2, 1))
title <- paste("GSE43494", "/", annotation(gset), " selected samples", 
    sep = "")
boxplot(ex, boxwex = 0.6, notch = T, main = title, outline = FALSE, 
    las = 2, col = fl)
legend("topleft", labels, fill = palette(), bty = "n") 
