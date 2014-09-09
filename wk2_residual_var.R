setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Regression-Models")
require(UsingR)
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

png('residual1.png')
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n) 
    lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)
dev.off()
png('residual2.png')
plot(diamond$carat, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
    lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)
dev.off()

# non-linear data
x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2);
png("residual3.png")
par(mfcol=c(1,2))
plot(x, y); abline(lm(y ~ x))
plot(x, resid(lm(y ~ x)));
abline(h = 0)
dev.off()

# Heteroskedasticity
x <- runif(100, 0, 6); y <- x + rnorm(100, mean = 0, sd = .001 * x);
png("residual4.png")
plot(x, y); abline(lm(y ~ x))
plot(x, resid(lm(y ~ x)));
abline(h = 0)
dev.off()

# residual variation
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma
sqrt(sum(resid(fit)^2) / (n - 2))

example(anscombe)
