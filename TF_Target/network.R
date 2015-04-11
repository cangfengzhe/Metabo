library(igraph)
library(reshape2)
corr_long <- melt(corr, na.rm = T) %>% 
  filter(Var1 != Var2)

graph<-graph.data.frame(corr_long, directed=F, vertices=NULL)

plot(graph)
