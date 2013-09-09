###############################################################################
#
# 1. Spline to fit a smooth function on melting curve data
# 2. 1st Derivative for melting data
#
###############################################################################
#setwd("/Users/xuh2/Documents/WorkDir/rMouseGT/Data")
setwd("/Users/li11/learningDoc/learningRmodeling/Data/")
# read the temperature list
tem <- read.table("MP02704.tem")
# read the melting fluorescent data matrix, each column is a sample well
mlt <- read.table("MP02704.mlt")

# define function to calculate 1st derivative
firstDerivative = function ( dt.vec, mltVec ) {
  # combine temperature list with melting data matrix
  dataMtrx <- cbind(dt.vec , mltVec) 
  # add column names
  colnames(dataMtrx) <- c("temp", "melt")
  # fit a smooth spline, and return a function describing it
  fx.spline <- splinefun(dataMtrx$temp, dataMtrx$melt)
  # get the negative of 1st derivative
  fx.neg1st <- -fx.spline(dataMtrx$temp, deriv = 1)
  return(fx.neg1st)
} # end of function

# for each melting fluorescent data column, calculate the negtive 1st derivative
# return a matrix
newMlt <- sapply(mlt, firstDerivative(tem))

# combined temperature list with melting data matrix
cur <- cbind(tem, newMlt)

# rename the column names
colnames(cur) <- c("Temp", "A01", "B01", "C01", "D01", "E01", "F01", "G01", "H01", "A02", "B02",
"C02", "D02", "E02", "F02", "G02", "H02", "A03", "B03", "C03", "D03", "E03", "F03", "G03",
"H03", "A04", "B04", "C04", "D04", "E04", "F04", "G04", "H04", "A05", "B05", "C05", "D05",
"E05", "F05", "G05", "H05", "A06", "B06", "C06", "D06", "E06", "F06", "G06", "H06", "A07",
"B07", "C07", "D07", "E07", "F07", "G07", "H07", "A08", "B08", "C08", "D08", "E08", "F08",
"G08", "H08", "A09", "B09", "C09", "D09", "E09", "F09", "G09", "H09", "A10", "B10", "C10",
"D10", "E10", "F10", "G10", "H10", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11",
"A12", "B12", "C12", "D12", "E12", "F12", "G12", "H12")
# transpose data frame
cur2 <- data.frame(t(cur))
# make the first row - temperature as column name
colnames(cur2) <- cur2[1, ]
# remove the first row - temperature
cur2 <- cur2[-1, ]
# read the genotyping map
gtm <- read.table("/Users/xuh2/Documents/WorkDir/rMouseGT/Data/genomap.txt")
# make the first column - well name as row name
rownames(gtm) <- gtm[, 1]
# remove the first column - well name
gtm <- gtm[, -1]
# merge two data frames by row names
allCmb <- merge(gtm, cur2, by = 0)

# format data frame
colnames(allCmb)[4] <- "Genotype"
allCmb[2:3] <- list(NULL)
# make the first column - well name as row name
rownames(allCmb) <- allCmb[, 1]
# remove the first column - well name
allCmb[1] <- NULL

# prepare train and test sets
index <- 1:nrow(allCmb)
odd_idx <- which(index%%2 == 1)
even_idx <- which(index%%2 == 0)
testset <- allCmb[odd_idx, ]
trainset <- allCmb[even_idx, ]
# remove genotype info from testset
testset[1] <- NULL

# use LDA to do machine learning
library(MASS)
modelLDA <- lda(Genotype ~ ., data = trainset)
predLDA <- predict(modelLDA, testset)
predLDA

# verify with manual genotype calls
allCmb[odd_idx,1]

# use SVM to do machine learning
library(e1071)
modelSVM <- svm(Genotype ~ ., data = trainset)
predSVM <- predict(modelSVM, testset)
predSVM
# do machine learning with Naive Bayesian
modelNB <- naiveBayes(Genotype ~ ., data = trainset)
predNB <- predict(modelNB, testset)
predNB

# use random forest to do machine learning
library(randomForest)
modelRF <- randomForest(Genotype ~ ., data = trainset)
predRF <- predict(modelRF, testset)
predRF
