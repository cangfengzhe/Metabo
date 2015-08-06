library(DCGL)
load('data/express_data_order.rdata')
D1 <- varianceBasedfilter(express_data_order, 0.05)
exprs_ga <- D1[, 1:228]
exprs_gyh <- D1[, 229: 396]

system.time({
DCp.res <- DCp(exprs_ga, exprs_gyh,
                    r.method = c("pearson", "spearman")[1],
                    link.method = c("qth", "rth", "percent")[1],
                    cutoff = 0.25,
                    N=100,
                    N.type = c("pooled", "gene_by_gene")[1],
                    q.method = c("BH", "holm", "hochberg", "hommel", "bonferroni","BY", "fdr")[1])

})

DCe.res <- DCe(exprs_ga, exprs_gyh,
                    link.method = c("qth", "rth", "percent")[1],
                    cutoff = 0.25,
                    r.method = c("pearson", "spearman")[1],
                    q.method = c("BH", "holm", "hochberg", "hommel","bonferroni", "BY", "fdr")[1],
                    nbins = 20, p = 0.1)

DCe.res$DCGs %>% View

DCe.res$DCGs[1:3, ]

save.image('./data/data.rdata')

DCsum.res <- DCsum(DCp.res, DCe.res,
                        DCpcutoff = 0.1,
                        DCecutoff = 0.1)
 

DCsum.res$DCGs %>% nrow
DCsum.res$DCLs %>% View
data(tf2target)
dcg <- DCsum.res$DCGs
dc_link <- DCsum.res$DCLs
dc_link1 <- dc_link %>% filter(cor.diff>0.5) 

nrow(dc_link1)

# Get the gene names that are mapped to an entrez gene identifier----

library(org.Hs.eg.db)
x <- org.Hs.egSYMBOL
# Get the gene symbol that are mapped to an entrez gene identifiers
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
entrez_symbol <- do.call('rbind', xx) %>% as.data.frame()
entrez_symbol[, 2] <- rownames(entrez_symbol)

entrez <- data.frame(dcg$DCG, stringsAsFactors  = F)
class(entrez$DCG) <- 'character'
colnames(entrez) <- 'DCG'
aa <- entrez %>% 
  left_join(entrez_symbol, by = c('DCG' = 'V2'))

View(aa)
class(entrez$DCG)

na.omit(aa) %>% nrow

gene1 <- unique(c(unique_link$gene1,unique_link$gene2))
length(gene1)
save.image('./data/data.rdata')


link <- dc_link[, c(1,2, 6)]
colnames(link) <- c('gene1', 'gene2', 'cor')
sapply(1:3, function(ii){
  link[,ii] <<- as.character(link[, ii])
})

library(igraph)
library(reshape2)
g <- graph.data.frame(link, directed = F)
adjacency <- get.adjacency(g, type = 'both')


library(MCL)
bb <- as.matrix(adjacency)
str(adjacency)
cc <- mcl(bb, addLoops = F, allow1 = T)
llply(1: nrow(link), .progress = 'text', function(ii){
  
  bb[link[ii,1], link[ii,2]] <<- bb[link[ii,2], link[ii,1]] <<- link[ii,3]
})
nrow(link)
save(bb, file = 'bb.rdata')
cc <- mcl(bb, addLoops = F, allow1 = T)
class(bb)
gc()

class(bb[,1])
dd <- as.data.frame(bb)
sapply(1:nrow(bb), function(ii){
  dd[, ii] <<- as.numeric(dd[,ii]) 
})
dd <- as.matrix(dd)
cc <- mcl(dd, addLoops = F, allow1 = T)
