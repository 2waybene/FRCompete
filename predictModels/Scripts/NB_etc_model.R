##=================================================
##	File name	: NB_etc_model.R
##	Author   	: Jianying Li
##	Comment	: initial coded by Hong Xu and 
##			  modified by Jianying Li
##=================================================


setwd("X:/myGit/FRCompete/predictModels/Data")
dataDir <- "X:/myGit/FRCompete/predictModels/Data"

setwd("/Users/li11/myGit/FRCompete/predictModels/Data")
dataDir <- "/Users/li11/myGit/FRCompete/predictModels/Data"
load ("Hong_processed_data.Rda")


##===========================================
##	Models in caret
##===========================================
library(caret)
TrainData <- allCmb[,-1]
TrainClasses <- allCmb[,1]
trt=trainControl(method='cv',number=10)

nbFit <- train(TrainData, TrainClasses,
                 method = "nb",
                 trControl = trt)
#ainControl(method = "boot"))
library(klaR)
nbFit2 <- NaiveBayes(TrainData, TrainClasses)
, usekernel=TRUE)

##==============================================
##	Inital NB model in e1071 works just fine
##==============================================
library(e1071)
trainNB = allCmb

modelNB2 <- naiveBayes(Genotype ~ ., data = trainNB)
predNB2 <- predict(modelNB2, trainNB)
predNB2

##=======Hong's code here...
##=============================================================
##	Split datat into training and testing
##=============================================================

# prepare data for caret training: separated labels and features
Ygtm <- allCmb[[1]] # convert sub data frame to vector
Xcur <- allCmb[-1] # get features without genotype labels

# split train and test sets (half/half)
library(caret)
set.seed(1)
inTrain <- createDataPartition(Ygtm, p = 1/2, list = FALSE)

trainX <- Xcur[inTrain,]
testX <- Xcur[-inTrain,]

trainY <- Ygtm[inTrain]
testY <- Ygtm[-inTrain]


##===========================================
##	Modeling here..
##===========================================

# train LDA model
ldaFit <- train( x = trainX, y = trainY,
                 method = "lda")
predLDA <- predict (ldaFit, testX)
predLDA



# train NB model
nbFit <- train( x = trainX, y = trainY,
                method ='nb',
                trControl=trainControl(method='cv',number=10))


trainNB <- allCmb[inTrain,]
testNB <- allCmb[-inTrain,]
testNB[1] <- NULL
library(klaR)
mdlNB <- NaiveBayes(Genotype ~ ., data = trainNB)


#Error in if (any(temp)) stop("Zero variances for at least one class in variables: ",  :
#  missing value where TRUE/FALSE needed


##==============================================
##	Inital NB model in e1071 works just fine
##==============================================
library(e1071)
modelNB2 <- naiveBayes(Genotype ~ ., data = trainNB)
predNB2 <- predict(modelNB2, testNB)
predNB2


# [1]  mut het het wt  het het wt  het wt  het wt  mut mut het het het het het het wt  het mut mut mut 
# [25] mut mut wt  het het het het wt  het wt  het mut het wt  het wt  het mut wt  het het
#Levels: Failed het mut wt
>

testY 
>[1] mut het    het    wt     het    het    wt     het    wt     wt     wt     mut    mut    het    het    het    het    het    het    wt     het    mut    mut    mut   
[25] mut    mut    wt     het    mut    het    het    Failed het    wt     het    mut    het    wt     het    wt     het    mut    wt     het    het   
Levels: Failed het mut wt
allCmb[-inTrain,1]

##=======================================
##	NB model in klaR library
##=======================================

ctrl = trainControl(method='cv',number=10)

nbFit <- train( x = trainX, y = trainY,
                method ='nb',
                trControl=ctrl )

colname.mod <- paste ("V", c (1:171), sep = "_")
colnames(trainX)<- colname.mod

nbFit <- train( trainX, trainY, method ='nb',  trControl=ctrl )

nbFit <- train( trainX, trainY, method ='knn',  preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))


trainNB <- allCmb[inTrain,]
testNB <- allCmb[-inTrain,]
testNB[1] <- NULL
library(klaR)
mdlNB <- NaiveBayes(Genotype ~ ., data = trainNB)


#Error in if (any(temp)) stop("Zero variances for at least one class in variables: ",  :
#  missing value where TRUE/FALSE needed

##================================================
##	NeuralNetwork model 
##	Failed as "class Failed does NOT have data
##================================================

nnetFit <- train(trainX, trainY,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)


