# 差异共表达基因

# source('http://bioconductor.org/biocLite.R')
# biocLite('EBcoexpress')
# library(EBcoexpress)
# vignette('EBcoexpress')
#
browseVignettes('EBcoexpress')


# -----------------
### R code from vignette source 'EBcoexpressVignette.Rnw'

###################################################
### code chunk number 1: foo
###################################################
options(keep.source = TRUE, width = 60)
foo <- packageDescription("EBcoexpress")


###################################################
### code chunk number 2: packageLoad
###################################################
library(EBcoexpress)
data(fiftyGenes) # A 50-by-125 expression matrix
tinyCond <- c(rep(1,228),rep(2,168))
tinyPat <- ebPatterns(c("1,1","1,2"))

class(express_data_order[1,2:10])
###################################################
### code chunk number 3: Dcreation
###################################################
colnames(express_data_order)
D <- makeMyD(express_data_order[1:10, ], tinyCond, useBWMC=F)

# aa <- cor(express_data_order[, 1:228] %>% t())

# cor(express_data_order[1, 1:228] %>% as.numeric(), express_data_order[2, 1:228] %>% as.numeric())

#cor(fiftyGenes[49,1:100], fiftyGenes[50, 1:100])
#cor(fiftyGenes[49,101:125], fiftyGenes[50, 101:125])


###################################################
### code chunk number 4: initialization
###################################################
set.seed(3)
initHP <- initializeHP(D, tinyCond)


###################################################
### code chunk number 5: lookAtInitHP
###################################################
print(initHP)


###################################################
### code chunk number 6: EMruns
###################################################
# zout <- ebCoexpressZeroStep(D, tinyCond, tinyPat, initHP)
oout <- ebCoexpressOneStep(D, tinyCond, tinyPat, initHP)
#oout$POSTPROBS[1,]

###################################################
### code chunk number 7: EMrunsNotRun (eval = FALSE)
###################################################
## fout <- ebCoexpressFullTCAECM(D, tinyCond, tinyPat, initHP)


###################################################
### code chunk number 8: pulloff
###################################################
result0 <- zout$POSTPROBS
result1 <- oout$POSTPROBS


###################################################
### code chunk number 9: pulloffNR (eval = FALSE)
###################################################
## resultF <- fout$POSTPROBS


###################################################
### code chunk number 10: f1a
###################################################
priorDiagnostic(D, tinyCond, zout, 2)


###################################################
### code chunk number 11: f1b
###################################################
priorDiagnostic(D, tinyCond, zout, 2)


###################################################
### code chunk number 12: f1c
###################################################
priorDiagnostic(D, tinyCond, oout, 1)


###################################################
### code chunk number 13: f1d
###################################################
priorDiagnostic(D, tinyCond, oout, 2)


###################################################
### code chunk number 14: grabbingPairs
###################################################
ppbDC1 <- 1-result1[,1]
crit_s <- crit.fun(result1[,1], 0.05)
kept_s <- ppbDC1[ppbDC1 >= crit_s]
kept_h <- ppbDC1[ppbDC1 >= 0.95]
klabs_s <- names(kept_s)
# DC pair names, under soft thresholding
klabs_h <- names(kept_h)
# DC pair names, under hard thresholding


###################################################
### code chunk number 15: TPgeneration
###################################################
ii <- c(rep(1:24, times=24:1), rep(26:49, times=24:1))
base1 <- 2:25
for(j in 3:25) base1 <- c(base1, j:25)
base2 <- 27:50
for(j in 28:50) base2 <- c(base2, j:50)
jj <- c(base1, base2)
TP <- mapply("X",ii,"~","X",jj,FUN=paste,sep="")
names(TP) <- NULL
numDC <- length(TP)


###################################################
### code chunk number 16: moreGrabbing
###################################################
nk_s <- length(kept_s) # No. taken as DC (soft)
nk_h <- length(kept_h) # No. taken as DC (hard)
nY_s <- sum(klabs_s %in% TP)
# Number of TP taken as DC under soft thresholding
nY_h <- sum(klabs_h %in% TP)
# Number of TP taken as DC under soft thresholding

(nk_s - nY_s)/nk_s           # Soft threshold Obs. FDR
(nk_h - nY_h)/nk_h           # Hard threshold Obs. FDR
nY_s/numDC                   # Soft threshold Obs. Power
nY_h/numDC                   # Hard threshold Obs. Power


###################################################
### code chunk number 17: rankThem
###################################################
hubs <- rankMyGenes(oout)
print(hubs)


###################################################
### code chunk number 18: newInit
###################################################
newInitHP <- initHP
newInitHP$G <- 3
newInitHP$MUS <- c(-0.5, 0, 0.5)
newInitHP$TAUS <- c(0.1, 0.2, 0.2)
newInitHP$WEIGHTS <- c(0.25, 0.5, 0.25)


###################################################
### code chunk number 19: dummy1 (eval = FALSE)
###################################################
## ebCoexpressMeta(DList, conditionsList, pattern, hpEstsList)


###################################################
### code chunk number 20: dummy2
###################################################
D1 <- D
D2 <- D
DList <- list(D1, D2)
cond1 <- tinyCond
cond2 <- tinyCond
conditionsList <- list(cond1, cond2)
pattern <- ebPatterns(c("1,1","1,2"))
initHP1 <- initHP
initHP2 <- initHP
out1 <- ebCoexpressZeroStep(D1, cond1, pattern, initHP1)
out2 <- ebCoexpressZeroStep(D2, cond2, pattern, initHP2)
hpEstsList <- list(out1$MODEL$HPS, out2$MODEL$HPS)

metaResults <- ebCoexpressMeta(
  DList, conditionsList, pattern, hpEstsList)


###################################################
### code chunk number 21: grab20
###################################################
twentyGeneNames <- dimnames(fiftyGenes)[[1]][c(1:10,26:35)]


###################################################
### code chunk number 22: f2a
###################################################
showNetwork(twentyGeneNames, D, condFocus = 1, gsep = "~",
            layout = "kamada.kawai", seed = 5, vertex.shape="circle",
            vertex.label.cex=1, vertex.color="white", edge.width=2,
            vertex.frame.color="black", vertex.size=20,
            vertex.label.color="black", vertex.label.family="sans")


###################################################
### code chunk number 23: f2b
###################################################
showNetwork(twentyGeneNames, D, condFocus = 2, gsep = "~",
            layout = "kamada.kawai", seed = 5, vertex.shape="circle",
            vertex.label.cex=1, vertex.color="white", edge.width=2,
            vertex.frame.color="black", vertex.size=20,
            vertex.label.color="black", vertex.label.family="sans")


###################################################
### code chunk number 24: f3a
###################################################
showNetwork(twentyGeneNames, D, condFocus = 1, gsep = "~",
            layout = "kamada.kawai", seed = 5, vertex.shape="circle",
            vertex.label.cex=1, vertex.color="white", edge.width=2,
            vertex.frame.color="black", vertex.size=20,
            vertex.label.color="black", vertex.label.family="sans",
            hidingThreshold=0.3)


###################################################
### code chunk number 25: f3b
###################################################
showNetwork(twentyGeneNames, D, condFocus = 2, gsep = "~",
            layout = "kamada.kawai", seed = 5, vertex.shape="circle",
            vertex.label.cex=1, vertex.color="white", edge.width=2,
            vertex.frame.color="black", vertex.size=20,
            vertex.label.color="black", vertex.label.family="sans",
            hidingThreshold=0.3)


###################################################
### code chunk number 26: fig4
###################################################
showPair("X1~X2", fiftyGenes, tinyCond, pch=20,
         xlim=c(-4,4), ylim=c(-4,4))



