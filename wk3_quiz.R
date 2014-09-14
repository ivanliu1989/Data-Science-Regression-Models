setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Regression-Models")
# q1
data(mtcars)
str(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
fit <- lm(mpg ~ cyl + wt - 1, data=mtcars)
summary(fit) 
attributes(fit)
fit$coefficients[1]-fit$coefficients[3]

# q2
fit2 <- lm(mpg ~ cyl - 1, data=mtcars)
summary(fit2)

# q3
fit1 <- lm(mpg ~ cyl + wt, data=mtcars)
fit3 <- lm(mpg ~ cyl + wt + I(cyl * wt), data=mtcars)
anova(fit1, fit3)

# q4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
lm(mpg ~ wt + factor(cyl), data = mtcars)

# q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit10 <- lm(y~x)
hatvalues(fit10)

# q6
dfbetas(fit10)

# q7