data <- read.csv("E:\\data\\matchWord.csv", header = F)
attach(data)
A <- data[V3 == 1, 1]
B <- data[V3 == 2, 1]
C <- data[V3 == 3, 1]
D <- data[V3 == 4, 1]
E <- data[V3 == 5, 1]
F <- data[V3 == 6, 1]
n = 0
word = NA
for (aa in 1:length(A)) {
    for (bb in 1:length(B)) {
        for (ee in 1:length(E)) {
            for (ff in 1:length(F)) {
                n <- n + 1
                word[n] <- paste(A[aa], B[bb], E[ee], F[ff], collapse = " ")
            }
        }
    }
}
geneWord <- word
proteinWord <- word
aa <- rbind(as.data.frame(A), as.data.frame(B))
A <- data.frame(A)
B <- data.frame(aa = B)
names(B) <- "A"
rbind(A, B)
save(geneWord, proteinWord, file = "word.rdata") 
