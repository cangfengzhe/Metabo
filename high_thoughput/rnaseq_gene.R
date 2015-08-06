library(airway)
dir <- system.file("extdata", package="airway", mustWork=TRUE)
list.files(dir)
sampleTable <- read_csv( file.path(dir,"sample_table.csv"))
filenames <- file.path(dir, paste0(sampleTable$Run, "_subset.bam"))

library("Rsamtools")
bamfiles <- BamFileList(filenames, yieldSize=2000000, index = character())


library("GenomicFeatures")
gtffile <- file.path(dir,"Homo_sapiens.GRCh37.75_subset.gtf")
(txdb <- makeTranscriptDbFromGFF(gtffile, format="gtf"))
genes <- exonsBy(txdb, by="gene")

library("GenomicAlignments")
flag <- scanBamFlag(isSecondaryAlignment=FALSE, isProperPair=TRUE)

param <- ScanBamParam(flag=flag)
## count reads overlapping a gene model; each read counted
## at most once; strand-unaware assay; paired-end reads;

se <- summarizeOverlaps(features=genes, reads=bamfiles,
                        mode="Union",
                        singleEnd=FALSE,
                        ignore.strand=TRUE,
                        fragments=TRUE
                        )

(colData(se) <- DataFrame(sampleTable))

library("DESeq2")

dds <- DESeqDataSet(se, design = ~ cell + dex)
dds$dex <- relevel(dds$dex, "untrt")
dds <- DESeq(dds)
res <- results(dds)
