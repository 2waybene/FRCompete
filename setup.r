data.dir   <- '~/projects/rface/data/'
train.file <- paste0(data.dir, 'training.csv')
test.file  <- paste0(data.dir, 'test.csv')
d.train <- read.csv(train.file, stringsAsFactors=F)

str(d.train)
im.train      <- d.train$Image
d.train$Image <- NULL
install.packages('doMC')
library(doMC)
registerDoMC()
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
    as.integer(unlist(strsplit(im, " ")))
}
str(im.train)
d.test  <- read.csv(test.file, stringsAsFactors=F)
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% {
    as.integer(unlist(strsplit(im, " ")))
}
d.test$Image <- NULL
save(d.train, im.train, d.test, im.test, file='data.Rd')
