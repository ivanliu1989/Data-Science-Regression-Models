setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Regression-Models")
library(UsingR)
data(diamond)
png("slm.png")
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(lm(price ~ carat, data = diamond), lwd = 2)
dev.off()

fit <- lm(price ~ carat, data = diamond)
coef(fit)

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
