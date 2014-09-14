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

