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

