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

png('least_sqr.png')
hist(galton$child,col="blue",breaks=100)
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)
dev.off()

# galton
png('galton.png')
plot(galton$parent, galton$child, pch=19, col='blue')
dev.off()

png('galton_freq.png')
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq, 
     xlab = "parent", ylab = "child")
dev.off()

myPlot <- function(beta){
    y <- galton$child - mean(galton$child)
    x <- galton$parent - mean(galton$parent)
    freqData <- as.data.frame(table(x, y))
    names(freqData) <- c("child", "parent", "freq")
    plot(
        as.numeric(as.vector(freqData$parent)), 
        as.numeric(as.vector(freqData$child)),
        pch = 21, col = "black", bg = "lightblue",
        cex = .15 * freqData$freq, 
        xlab = "parent", 
        ylab = "child"
    )
    abline(0, beta, lwd = 3)
    points(0, 0, cex = 2, pch = 19)
    mse <- mean( (y - beta * x)^2 )
    title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
png('beta.png')
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))
dev.off()

# solution
lm(I(child-mean(child))~I(parent-mean(parent))-1,data=galton)

y <- galton$child
x <- galton$parent
beta1 <- cor(y,x)*sd(y)/sd(x)
beta0 <- mean(y)-beta1*mean(x)
rbind(c(beta0,beta1),coef(lm(y~x)))
    #reversing
beta1 <- cor(y,x)*sd(x)/sd(y)
beta0 <- mean(x)-beta1*mean(y)
rbind(c(beta0,beta1),coef(lm(x~y)))

# normalizing data
data(father.son)
y<- (father.son$sheight - mean(father.son$sheight))/sd(father.son$sheight)
x<- (father.son$fheight - mean(father.son$fheight))/sd(father.son$fheight)
rho <- cor(x,y)
myplot <- function(x,y){
    plot(x,y,xlab="Father's height,normalizied",
    ylab="Son's height, normailized",
    xlim=c(-3,3),ylim=c(-3,3),
    bg='lightblue',col='black',cex=1.1,pch=21,frame=F)
}
png('nor_data.png')
myplot(x,y)
abline(0,1)
abline(0,rho,lwd=2)
abline(0,1/rho,lwd=2)
abline(h=0);abline(v=0)
dev.off()

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(x~y)
data(mtcars)
lm(mpg~wt, data=mtcars)
head(mtcars)
beta1 <- cor(y,x)*sd(x)/sd(y)
beta0 <- mean(x)-beta1*mean(y)
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
y <- x-mean(x)
scale(y)
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)
y<-x
