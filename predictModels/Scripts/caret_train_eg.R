
#######################################
## Classification Example

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

knnFit2 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10, 
                 trControl = trainControl(method = "boot"))


knnFit3 <- train(TrainData, TrainClasses,
                 method = "nb",
                 trControl = trainControl(method = "boot"))


library(MASS)
nnetFit <- train(TrainData, TrainClasses,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

str(TrainData)
str(TrainClasses)

nnetFit <- train(trainX, trainY,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

trainX.short <- trainX[,c(1:4)]
str(trainY)

nnetFit <- train(trainX.short, trainY,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)

