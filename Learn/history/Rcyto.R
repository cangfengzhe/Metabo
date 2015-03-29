library(RCytoscape)
g <- new("graphNEL", edgemode = "directed")
# NA
g <- initNodeAttribute(graph = g, attribute.name = "moleculeType", 
    attribute.type = "char", default.value = "undefined")
g <- initNodeAttribute(graph = g, "lfc", "numeric", 0)

g <- addNode("A", g)
g <- addNode("B", g)
g <- addNode("C", g)
g = addEdge("A", "B", g)
g = addEdge("A", "C", g)
nodeData(g, "A", "moleculeType") <- "kinase"
nodeData(g, "B", "moleculeType") <- "TF"
nodeData(g, "C", "moleculeType") <- "cytokine"
nodeData(g, "A", "lfc") <- -1.2
nodeData(g, "B", "lfc") <- 1.8
nodeData(g, "C", "lfc") <- 3.2
cw <- new.CytoscapeWindow("vignett", graph = g)

layout(cw, layout.name = "grid")
setDefaultNodeShape(cw, "octagon")
setDefaultNodeColor(cw, "#AAFF88")

# NA

displayGraph(cw)
# NA
setNodeColorRule(cw, "lfc", c(-3, 0, 3), c("#00FF00", "#FFFFFF", 
    "#FF0000"), mode = "interpolate")
# NA
redraw(cw) 
