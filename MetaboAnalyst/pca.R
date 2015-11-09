library(psych)
install.packages('psych')
pc <- principal(USJudgeRatings[, -1], nfactors = 5, scores = T, rotate = 'none')
USJudgeRatings[, -1] %>% dim
USJudgeRatings[, -1] %>% as.matrix() %*% pc$weights

pc$values[1] %>% sqrt * eign_value$vectors[,1]
cor(pc$weights[1:5,1],
pc$loadings[1:5,1]
)

pc$loadings
cor_data <- cor(USJudgeRatings[, -1])
eign_value <- eigen(cor_data, T)
eign_value$vectors[,1]
pc$values[1] %>% sqrt() * pc$loadings[1,1]
loadings

# 验证principal
r <- USJudgeRatings[, -1]

