setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Regression-Models")
library(datasets); data(swiss); require(stats); require(graphics)
png("swiss_pairs.png")
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))
dev.off()

summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef
png("comp_adj_unadj.png")
par(mfrow = c(1, 2))
plot(x1, y, pch=21,col="black",bg=topo.colors(n)[x2], frame = FALSE, cex = 1.5)
title('Unadjusted, color is X2')
abline(lm(y ~ x1), lwd = 2)
plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), pch = 21, col = "black", bg = "lightblue", frame = FALSE, cex = 1.5)
title('Adjusted')
abline(0, coef(lm(y ~ x1 + x2))[2], lwd = 2)
dev.off()
par(mfrow = c(1, 1))

z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)

require(datasets);data(InsectSprays)
require(stats); require(graphics)
png("insect_sprays.png")
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")
dev.off()

summary(lm(count ~ spray, data = InsectSprays))$coef
summary(lm(count ~
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F'))
           , data = InsectSprays))$coef
lm(count ~
       I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
       I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
       I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays)
summary(lm(count ~ spray - 1, data = InsectSprays))$coef

#Reordering the levels
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

fit <- lm(count ~ spray, data = InsectSprays) #A is ref
bbmbc <- coef(fit)[2] - coef(fit)[3] #B - C
temp <- summary(fit)
se <- temp$sigma * sqrt(temp$cov.unscaled[2, 2] + temp$cov.unscaled[3,3] - 2 *temp$cov.unscaled[2,3])
t <- (bbmbc) / se
p <- pt(-abs(t), df = fit$df)
out <- c(bbmbc, se, t, p)
names(out) <- c("B - C", "SE", "T", "P")
round(out, 3)

download.file("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv?profile=text&filter=COUNTRY:*;SEX:*","hunger.csv",method="curl")
hunger <- read.csv("hunger.csv")
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)
lm1 <- lm(hunger$Numeric ~ hunger$Year)
png("hunger.png")
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
lines(hunger$Year,lm1$fitted,lwd=3,col="darkgrey")
dev.off()

png("hunger_fm.png")
lmM <- lm(hunger$Numeric[hunger$Sex=="Male"] ~ hunger$Year[hunger$Sex=="Male"])
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"] ~ hunger$Year[hunger$Sex=="Female"])
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
lines(hunger$Year[hunger$Sex=="Male"],lmM$fitted,col="black",lwd=3)
lines(hunger$Year[hunger$Sex=="Female"],lmF$fitted,col="red",lwd=3)
dev.off()

lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2] ),col="black",lwd=3)

lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex + hunger$Sex*hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2] +lmBoth$coeff[4]),col="black",lwd=3)

summary(lmBoth)
