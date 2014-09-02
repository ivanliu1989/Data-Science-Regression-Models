library(swirl)
swirl()
install_from_swirl("Regression Models")
Ivan
plot(child~parent,galton)
plot(jitter(child,4)~parent,galton)
regrline <- lm(child~parent, galton)
abline(regrline,lwd=3,col='red')
summary(regrline)

# course info
regmods-006
ivan.liuyanfeng@gmail.com
5WmqpA6U2F
###
fit <- lm(child~parent, galton)
summary(fit)
mean(fit$residuals)
cov(fit$residuals, galton$parent)
ols.ic <- fit$coef[1]
ols.slope <- fit$coef[2]
lhs-rhs
all.equal(lhs,rhs)
varChild <- var(galton$child)
varRes <- var(fit$residuals)
varEst <- var(est(ols.slope, ols.ic))
all.equal(varChild, varRes+varEst)

efit <- lm(accel~mag+dist, attenu)
mean(efit$residuals)
cov(efit$residuals, attenu$mag)
cov(efit$residuals, attenu$dist)

sqrt(sum(fit$residuals^2) / (n - 2))
summary(fit)$sigma
sqrt(deviance(fit)/(n-2))

mu <- mean(galton$child)
sTot <- sum((galton$child-mu)^2)
sRes <- deviance(fit)
1-sRes/sTot
summary(fit)$r.squared
cor(galton$child,galton$parent)^2

ones <- rep(1,nrow(galton))
lm(child~ones+parent-1,galton)
lm(child ~ parent, galton)
lm(child ~ 1, galton)
