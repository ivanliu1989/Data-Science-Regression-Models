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

View(trees)
fit <- lm(Volume ~ Girth + Height + Constant -1, trees)
trees2 <- eliminate("Girth", trees)
head(trees2)
fit2 <- lm(Volume ~ Height + Constant -1, trees2)
lapply(list(fit, fit2), coef)

all <- lm(Fertility ~ . , data=swiss)
summary(all)
summary(lm(Fertility ~ Agriculture, swiss))
cor(swiss$Examination,swiss$Education)
cor(swiss$Agriculture,swiss$Education)
makelms()

ec <- swiss$Examination+swiss$Catholic
efit <- lm(Fertility ~ . + ec, swiss)
all$coefficients-efit$coefficients

dim(InsectSprays)
head(InsectSprays,15)
summary(InsectSprays)
sapply(InsectSprays, class)
fit <- lm(count~spray,InsectSprays)
est <- summary(fit)$coef[,1]
mean(sB)
nfit <- lm(count~spray -1 ,InsectSprays)
summary(nfit)$coef
spray2 <- relevel(InsectSprays$spray,"C")
fit2 <- lm(count~spray2,InsectSprays)
summary(fit2)$coef
mean(sC)
(fit$coef[2]-fit$coef[3])/1.6011


dim(hunger)
names(hunger)
fit <- lm(hunger$Numeric ~ hunger$Year)
summary(fit)$coef
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"] ~ hunger$Year[hunger$Sex=="Female"]) 
lmM <- lm(hunger$Numeric[hunger$Sex=="Male"] ~ hunger$Year[hunger$Sex=="Male"])
lmBoth <- lm(Numeric ~ Year +Sex, hunger)
summary(lmBoth)
lmInter <- lm(Numeric ~ Year+Sex+Sex*Year, hunger)
summary(lmInter)
