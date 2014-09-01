setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Regression-Models")
library(UsingR)
data(galton)
png('chi_par.png')
par(mfrow=c(1,2))
hist(galton$child,col='blue',breaks=100)
hist(galton$parent,col='blue',breaks=100)
dev.off()

# Experiment
library(manipulate)
par(mfrow=c(1,1))
myHist <- function(mu){
    hist(galton$child,col="blue",breaks=100)
    lines(c(mu, mu), c(0, 150),col="red",lwd=5)
    mse <- mean((galton$child - mu)^2)
    text(63, 150, paste("mu = ", mu))
    text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
