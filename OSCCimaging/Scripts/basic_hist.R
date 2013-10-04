opar <- par()

op <- par(mfrow=c(2, 2))
# Simple Histogram
hist(mtcars$mpg)

# Colored Histogram with Different Number of Bins
hist(mtcars$mpg, breaks=12, col="red")


# Colored Histogram with Different Number of Bins
hist(mtcars$mpg, breaks=20, col="blue")

# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- mtcars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

par(opar)

?hist
xfit
length(mtcars$mpg)
