## just a test
#install.packages('reshape2')
#install.package('e1071')
#library('reshape2')
#library('e1071')

#load('data.Rd')
#library(doMC);
#registerDoMC();

testNum <- 100;
#testNum <- nrow(d.train)
im1 <- im.train[1 : testNum,];
train1 <- d.train[1: testNum,];
#test1 <- im.test[1:3,];
#test1 <- im.test[4200:4300,];
test1 <- im.train[4264:4265,];

colPred <- function(name) {
   mySvm <- svm(im1, train1[,name]);		
   return(predict(mySvm, test1));
}

num <- nrow(test1)
imageIds <- c(1 : num)


example.submission <- read.csv(paste0(data.dir, 'submissionFileFormat.csv'))
example.submission$Location <- NULL
submission <- example.submission;
submission <- transform(submission, FeatureName = as.character(FeatureName))
submission$Location <- NULL
#$memory restriction, so I use 1000 first
#name <- "left_eye_center_x";
mainMatrix <- NULL
result <- NULL

mainMatrix <- foreach (name = names(train1), .combine=rbind) %dopar% {
	predResult <- NULL
	predResult <- colPred(name);

	colNames <- rep(name, num)
	lst <- c(imageIds, colNames, predResult)
	matrix(lst, nrow=num, ncol=3)
}
colnames(mainMatrix) <- c("ImageId", "FeatureName", "Location")

result = data.frame(mainMatrix)			
result <- transform(result, ImageId = as.integer(ImageId));
result <- transform(result, FeatureName = as.character(FeatureName));

submission <- merge(submission, result, 
		by = intersect(names(submission), names(result)),
		all.x=TRUE, all.y=FALSE, sort=FALSE)
submission <- submission[c(3,1,2,4)]
write.csv(submission, file="a.csv", quote=F, row.names=F)

