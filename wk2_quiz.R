setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Regression-Models")
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y~x)
summary(fit)
fit$residual
sd(fit$residual)

data(mtcars)
fit2 <- lm(mpg~wt, data=mtcars)
head(mtcars)
summary(fit2)
?mtcars
head(mtcars)
predict(fit2, wt=3)
fit2$residual
