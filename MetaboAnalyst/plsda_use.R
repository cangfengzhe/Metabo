library(caret)
vignette('caret')
data(mdrr)
set.seed(1)
library(mlbench)
data(Sonar)
inTrain <- createDataPartition(y = Sonar$Class, 
                               ## the outcome data are needed
                               p = .75, 
                               ## The percentage of data in the 
                               ## training set
                               list = FALSE)
## The format of the results

## The output is a set of integers for the rows of Sonar 
## that belong in the training set.
str(inTrain)
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

plsFit <- train(Class ~ ., 
                data = training,
                method = "pls",
                ## Center and scale the predictors for the training 
                ## set and all future samples.
                preProc = c("center", "scale"),
                matrix = 'ROC')

plsClasses <- predict(plsFit, newdata = testing)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
confusionMatrix(plsClasses, testing$Class)
print(plot(plsFit))


# lda

ldaFit <- train(Class ~ ., 
                data = training,
                method = "lda",
                ## Center and scale the predictors for the training 
                ## set and all future samples.
                preProc = c("center", "scale"),
                matrix = 'ROC')

ldaClasses <- predict(ldaFit, newdata = testing)
ldaProbs <- predict(ldaFit, newdata = testing, type = "prob")
ggplot(ldaFit, metric = "Kappa")

inTrain <- sample(seq(along = mdrrClass), 450)




data(mdrr)
set.seed(1)
inTrain <- sample(seq(along = mdrrClass), 450)

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training)

trainDescr <- predict(preProcValues, training)
testDescr <- predict(preProcValues, test)

useBayes   <- plsda(trainDescr, trainMDRR, ncomp = 5,
                    probMethod = "Bayes")

y = trainMDRR
obsLevels <- levels(y)
oldY <- y
y <- class2ind(y)
trainDescr <- as.matrix(trainDescr)
tmpData <- data.frame(n = paste("row", 1:nrow(y), sep = ""))
tmpData$y <- y
tmpData <- cbind(tmpData, trainDescr)
# tmpData$x <- trainDescr
ncomp=2
obsLevels <- levels(oldY)
out <- pls::plsr(y ~ x, data = tmpData, ncomp = ncomp)
out$obsLevels <- obsLevels
class(out) <- 'mvr'
testDescr <- as.matrix(testDescr)
tmpPred <- predict(out, newdata = testDescr)[,,ncomp,drop = FALSE]

predict(out, testDescr)

useSoftmax <- plsda(trainDescr, trainMDRR, ncomp = 5)

ncomp=2
class(trainDescr) <- 'mvr'
tmpPred <- predict(useSoftmax,testDescr, type = 'raw')


confusionMatrix(predict(useBayes, testDescr),
                testMDRR)

confusionMatrix(predict(useSoftmax, testDescr),
                testMDRR)

histogram(~predict(useBayes, testDescr, type = "prob")[,"Active",]
          | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))
histogram(~predict(useSoftmax, testDescr, type = "prob")[,"Active",]
          | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))


## different sized objects are returned
length(predict(useBayes, testDescr))
dim(predict(useBayes, testDescr, ncomp = 1:3))
dim(predict(useBayes, testDescr, type = "prob"))
dim(predict(useBayes, testDescr, type = "prob", ncomp = 1:3))

## Using spls:
## (As of 11/09, the spls package now has a similar function with
## the same mane. To avoid conflicts, use caret:::splsda to 
## get this version)
