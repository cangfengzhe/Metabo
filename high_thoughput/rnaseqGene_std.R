## ----style, echo=FALSE, message=FALSE, warning=FALSE, results="asis"-----
library("BiocStyle")
library("knitr")
options(width=100)
opts_chunk$set(message = FALSE, error = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
library("airway")

## ------------------------------------------------------------------------
dir <- system.file("extdata", package="airway", mustWork=TRUE)

## ------------------------------------------------------------------------
list.files(dir)

## ------------------------------------------------------------------------
csvfile <- file.path(dir,"sample_table.csv")
(sampleTable <- read.csv(csvfile,row.names=1))

## ------------------------------------------------------------------------
filenames <- file.path(dir, paste0(sampleTable$Run, "_subset.bam"))

## ------------------------------------------------------------------------
library("Rsamtools")
bamfiles <- BamFileList(filenames, yieldSize=2000000)

## ------------------------------------------------------------------------
seqinfo(bamfiles[1])

## ------------------------------------------------------------------------
library("GenomicFeatures")

## ------------------------------------------------------------------------
gtffile <- file.path(dir,"Homo_sapiens.GRCh37.75_subset.gtf")
(txdb <- makeTranscriptDbFromGFF(gtffile, format="gtf"))

## ------------------------------------------------------------------------
(genes <- exonsBy(txdb, by="gene"))

## ------------------------------------------------------------------------
library("GenomicAlignments")

## ------------------------------------------------------------------------
se <- summarizeOverlaps(features=genes, reads=bamfiles,
                        mode="Union",
                        singleEnd=FALSE,
                        ignore.strand=TRUE,
                        fragments=TRUE )

## ----sumexp, echo=FALSE--------------------------------------------------
par(mar=c(0,0,0,0))
plot(1,1,xlim=c(0,100),ylim=c(0,100),bty="n",
     type="n",xlab="",ylab="",xaxt="n",yaxt="n")
polygon(c(45,80,80,45),c(10,10,70,70),col="pink",border=NA)
polygon(c(45,80,80,45),c(68,68,70,70),col="pink3",border=NA)
text(62.5,40,"assay(s)")
text(62.5,30,"e.g. 'counts'")
polygon(c(20,40,40,20),c(10,10,70,70),col="skyblue",border=NA)
polygon(c(20,40,40,20),c(68,68,70,70),col="skyblue3",border=NA)
text(30,40,"rowRanges")
polygon(c(45,80,80,45),c(75,75,90,90),col="palegreen",border=NA)
polygon(c(45,47,47,45),c(75,75,90,90),col="palegreen3",border=NA)
text(62.5,82.5,"colData")

## ------------------------------------------------------------------------
se
head(assay(se))
colSums(assay(se))
colData(se)
rowRanges(se)

## ------------------------------------------------------------------------
str(metadata(rowRanges(se)))

## ------------------------------------------------------------------------
(colData(se) <- DataFrame(sampleTable))

## ------------------------------------------------------------------------
data("airway")
se <- airway

## ------------------------------------------------------------------------
round( colSums(assay(se)) / 1e6, 1 )

## ------------------------------------------------------------------------
colData(se)

## ------------------------------------------------------------------------
library("DESeq2")

## ------------------------------------------------------------------------
dds <- DESeqDataSet(se, design = ~ cell + dex)

## ------------------------------------------------------------------------
countdata <- assay(se)
head(countdata)

## ------------------------------------------------------------------------
coldata <- colData(se)

## ------------------------------------------------------------------------
(ddsMat <- DESeqDataSetFromMatrix(countData = countdata,
                                 colData = coldata,
                                 design = ~ cell + dex))

## ------------------------------------------------------------------------
rld <- rlog(dds)
head(assay(rld))

## ----rldplot, fig.width=8, fig.height=4----------------------------------
par( mfrow = c( 1, 2 ) )
dds <- estimateSizeFactors(dds)
plot(log2( 1 + counts(dds, normalized=TRUE)[ , 1:2] ),
     pch=16, cex=0.3)
plot(assay(rld)[ , 1:2],
     pch=16, cex=0.3)

## ------------------------------------------------------------------------
sampleDists <- dist( t( assay(rld) ) )
sampleDists

## ------------------------------------------------------------------------
library("pheatmap")
library("RColorBrewer")

## ----distheatmap, fig.width=8--------------------------------------------
sampleDistMatrix <- as.matrix( sampleDists )
rownames(sampleDistMatrix) <- paste( rld$dex, rld$cell, sep="-" )
colnames(sampleDistMatrix) <- NULL
colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
pheatmap(sampleDistMatrix,
         clustering_distance_rows=sampleDists,
         clustering_distance_cols=sampleDists,
         col=colors)

## ------------------------------------------------------------------------
library("PoiClaClu")
poisd <- PoissonDistance(t(counts(dds)))

## ----poisdistheatmap, fig.width=8----------------------------------------
samplePoisDistMatrix <- as.matrix( poisd$dd )
rownames(samplePoisDistMatrix) <- paste( rld$dex, rld$cell, sep="-" )
colnames(samplePoisDistMatrix) <- NULL
pheatmap(samplePoisDistMatrix,
         clustering_distance_rows=poisd$dd,
         clustering_distance_cols=poisd$dd,
         col=colors)

## ----plotpca, fig.width=6, fig.height=4.5--------------------------------
plotPCA(rld, intgroup = c("dex", "cell"))

## ------------------------------------------------------------------------
(data <- plotPCA(rld, intgroup = c( "dex", "cell"), returnData=TRUE))
percentVar <- round(100 * attr(data, "percentVar"))

## ------------------------------------------------------------------------
library("ggplot2")

## ----ggplotpca, fig.width=6, fig.height=4.5------------------------------
qplot(PC1, PC2, color=dex, shape=cell, data=data) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance"))

## ----mdsrlog, fig.width=6, fig.height=4.5--------------------------------
mds <- data.frame(cmdscale(sampleDistMatrix))
mds <- cbind(mds, as.data.frame(colData(rld)))
qplot(X1,X2,color=dex,shape=cell,data=mds)

## ----mdspois, fig.width=6, fig.height=4.5--------------------------------
mds <- data.frame(cmdscale(samplePoisDistMatrix))
mds <- cbind(mds, as.data.frame(colData(dds)))
qplot(X1,X2,color=dex,shape=cell,data=mds)

## ------------------------------------------------------------------------
dds$dex <- relevel(dds$dex, "untrt")

## ------------------------------------------------------------------------
dds <- DESeq(dds)

## ------------------------------------------------------------------------
(res <- results(dds))

## ------------------------------------------------------------------------
mcols(res, use.names=TRUE)

## ------------------------------------------------------------------------
summary(res)

## ------------------------------------------------------------------------
res.05 <- results(dds, alpha=.05)
table(res.05$padj < .05)

## ------------------------------------------------------------------------
results(dds, contrast=c("cell", "N061011", "N61311"))

## ------------------------------------------------------------------------
sum(res$pvalue < 0.05, na.rm=TRUE)
sum(!is.na(res$pvalue))

## ------------------------------------------------------------------------
sum(res$padj < 0.1, na.rm=TRUE)

## ------------------------------------------------------------------------
resSig <- subset(res, padj < 0.1)
head(resSig[ order( resSig$log2FoldChange ), ])

## ------------------------------------------------------------------------
head(resSig[ order( -resSig$log2FoldChange ), ])

## ----plotcounts, fig.width=5, fig.height=5-------------------------------
topGene <- rownames(res)[which.min(res$padj)]
plotCounts(dds, gene=topGene, intgroup=c("dex"))

## ----ggplotcountsjitter, fig.height=5------------------------------------
data <- plotCounts(dds, gene=topGene, intgroup=c("dex","cell"), returnData=TRUE)
ggplot(data, aes(x=dex, y=count, color=cell)) +
  scale_y_log10() + 
  geom_point(position=position_jitter(width=.1,height=0))

## ----ggplotcountsdot, fig.height=5---------------------------------------
ggplot(data, aes(x=dex, y=count, fill=dex)) +
  scale_y_log10() + 
  geom_dotplot(binaxis="y", stackdir="center")

## ----ggplotcountsgroup, fig.height=5-------------------------------------
ggplot(data, aes(x=dex, y=count, color=cell, group=cell)) +
  scale_y_log10() + 
  geom_point() + geom_line()

## ----plotma--------------------------------------------------------------
plotMA(res, ylim=c(-5,5))

## ----plotma2-------------------------------------------------------------
plotMA(res, ylim=c(-5,5))
with(res[topGene, ], {
  points(baseMean, log2FoldChange, col="dodgerblue", cex=2, lwd=2)
  text(baseMean, log2FoldChange, topGene, pos=2, col="dodgerblue")
})

## ----plotdispests--------------------------------------------------------
plotDispEsts(dds)

## ----histpvalue----------------------------------------------------------
hist(res$pvalue, breaks=20, col="grey50", border="white")

## ----histpvalue2---------------------------------------------------------
hist(res$pvalue[res$baseMean > 1], breaks=20, col="grey50", border="white")

## ------------------------------------------------------------------------
library("genefilter")
topVarGenes <- head(order(-rowVars(assay(rld))),20)

## ----genescluster--------------------------------------------------------
mat <- assay(rld)[ topVarGenes, ]
mat <- mat - rowMeans(mat)
df <- as.data.frame(colData(rld)[,c("cell","dex")])
pheatmap(mat, annotation_col=df)

## ----sensitivityovermean, fig.height=4-----------------------------------
# create bins using the quantile function
qs <- c(0, quantile(res$baseMean[res$baseMean > 0], 0:7/7))
# cut the genes into the bins
bins <- cut(res$baseMean, qs)
# rename the levels of the bins using the middle point
levels(bins) <- paste0("~",round(.5*qs[-1] + .5*qs[-length(qs)]))
# calculate the ratio of $p$ values less than .01 for each bin
ratios <- tapply(res$pvalue, bins, function(p) mean(p < .01, na.rm=TRUE))
# plot these ratios
barplot(ratios, xlab="mean normalized count", ylab="ratio of small p values")

## ----filterthreshold-----------------------------------------------------
attr(res,"filterThreshold")
plot(attr(res,"filterNumRej"),type="b",
     xlab="quantiles of 'baseMean'",
     ylab="number of rejections")

## ------------------------------------------------------------------------
library("AnnotationDbi")
library("org.Hs.eg.db")

## ------------------------------------------------------------------------
columns(org.Hs.eg.db)

## ------------------------------------------------------------------------
convertIDs <- function( ids, from, to, db, ifMultiple=c("putNA", "useFirst")) {
  stopifnot( inherits( db, "AnnotationDb" ) )
  ifMultiple <- match.arg( ifMultiple )
  suppressWarnings( selRes <- AnnotationDbi::select(
    db, keys=ids, keytype=from, columns=c(from,to) ) )
  if ( ifMultiple == "putNA" ) {
    duplicatedIds <- selRes[ duplicated( selRes[,1] ), 1 ]
    selRes <- selRes[ ! selRes[,1] %in% duplicatedIds, ]
  }
  return( selRes[ match( ids, selRes[,1] ), 2 ] )
}

## ------------------------------------------------------------------------
res$hgnc_symbol <- convertIDs(row.names(res), "ENSEMBL", "SYMBOL", org.Hs.eg.db)
res$entrezgene <- convertIDs(row.names(res), "ENSEMBL", "ENTREZID", org.Hs.eg.db)

## ------------------------------------------------------------------------
resOrdered <- res[order(res$pvalue),]
head(resOrdered)

## ----eval=FALSE----------------------------------------------------------
## write.csv(as.data.frame(resOrdered), file="results.csv")

## ------------------------------------------------------------------------
(resGR <- results(dds, format="GRanges"))
resGR$symbol <- convertIDs(names(resGR), "ENSEMBL", "SYMBOL", org.Hs.eg.db)

## ------------------------------------------------------------------------
library("Gviz")

## ------------------------------------------------------------------------
window <- resGR[topGene] + 1e6
strand(window) <- "*"
hasLFC <- !is.na(resGR$log2FoldChange)
resGRsub <- resGR[resGR %over% window & hasLFC]
naOrDup <- is.na(resGRsub$symbol) | duplicated(resGRsub$symbol)
resGRsub$group <- ifelse(naOrDup, names(resGRsub), resGRsub$symbol)

## ------------------------------------------------------------------------
sig <- factor(ifelse(is.na(resGRsub$padj) | resGRsub$padj > .1,"notsig","sig"))

## ----gvizplot------------------------------------------------------------
options(ucscChromosomeNames=FALSE)
g <- GenomeAxisTrack()
a <- AnnotationTrack(resGRsub, name="gene ranges", feature=sig)
d <- DataTrack(resGRsub, data="log2FoldChange", baseline=0,
               type="h", name="log2 fold change", strand="+")
plotTracks(list(g,d,a), groupAnnotation="group", notsig="lightblue", sig="pink")

## ------------------------------------------------------------------------
library("sva")

## ------------------------------------------------------------------------
idx <- rowMeans(counts(dds)) > 1
dat <- counts(dds)[idx,]
mod <- model.matrix(~ dex, colData(dds))
mod0 <- model.matrix(~ 1, colData(dds))
svseq <- svaseq(dat, mod, mod0, n.sv=2)
svseq$sv

## ----svaplot-------------------------------------------------------------
par(mfrow=c(2,1),mar=c(5,5,1,1))
stripchart(svseq$sv[,1] ~ dds$cell,vertical=TRUE)
abline(h=0)
stripchart(svseq$sv[,2] ~ dds$cell,vertical=TRUE)
abline(h=0)

## ------------------------------------------------------------------------
ddssva <- dds
ddssva$SV1 <- svseq$sv[,1]
ddssva$SV2 <- svseq$sv[,2]
design(ddssva) <- ~ SV1 + SV2 + dex
ddssva <- DESeq(ddssva)
head(results(ddssva), 4)

## ------------------------------------------------------------------------
library("fission")
data("fission")
ddsTC <- DESeqDataSet(fission, ~ strain + minute + strain:minute)

## ------------------------------------------------------------------------
ddsTC <- DESeq(ddsTC, test="LRT", reduced = ~ strain + minute)
resTC <- results(ddsTC)
resTC$symbol <- mcols(ddsTC)$symbol
head(resTC[order(resTC$pvalue),],4)

## ----fissioncounts-------------------------------------------------------
data <- plotCounts(ddsTC, which.min(resTC$pvalue), 
                   intgroup=c("minute","strain"), returnData=TRUE)
ggplot(data, aes(x=minute, y=count, color=strain, group=strain)) + 
  geom_point() + stat_smooth(se=FALSE,method="loess") +  scale_y_log10()

## ------------------------------------------------------------------------
resultsNames(ddsTC)
res30 <- results(ddsTC, name="strainmut.minute30", test="Wald")
res30[which.min(resTC$pvalue),]

## ------------------------------------------------------------------------
betas <- coef(ddsTC)
colnames(betas)

## ----fissionheatmap------------------------------------------------------
library("pheatmap")
topGenes <- head(order(resTC$pvalue),20)
mat <- betas[topGenes, -c(1,2)]
thr <- 3 # threshold for plotting
mat[mat < -thr] <- -thr
mat[mat > thr] <- thr
pheatmap(mat, breaks=seq(from=-thr, to=thr, length=101),
         cluster_col=FALSE)

## ------------------------------------------------------------------------
sessionInfo()

