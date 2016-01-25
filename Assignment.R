library(datasets)
head(mtcars)

fit <- lm(mpg~., data = mtcars)
library("car")
vif(fit)

fit01 <- lm(mpg ~ factor(am), data = mtcars)

fit11 <- lm(mpg ~ factor(cyl) + factor(am), data = mtcars)
fit12 <- lm(mpg ~ factor(cyl) + factor(am) + disp, data = mtcars)
fit13 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp, data = mtcars)
fit14 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp + wt, data = mtcars)
fit15 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp + wt + qsec, data = mtcars)
fit16 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp + wt + qsec + carb, data = mtcars)

fit17 <- lm(mpg ~ factor(cyl) + factor(am) + disp + wt, data = mtcars)
fit14 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp + wt, data = mtcars)
fit18 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp, data = mtcars)

anova(fit11, fit12, fit13, fit14, fit15, fit16)
anova(fit15, fit14, fit16, fit17 )
anova(fit14, fit18)

fit5  <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp, data = mtcars)
fit21 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp + qsec, data = mtcars)
fit22 <- lm(mpg ~ factor(cyl) + factor(am) + disp + hp + gear, data = mtcars)
anova(fit5, fit22)

par(mfrow=c(2,2), mar=c(4,4,4,2))

plot(mtcars$mpg ~ mtcars$cyl, main="Mileage Vs No. of cylinders", xlab="No. of cylinders", ylab = "Mileage", col="red")
plot(mtcars$mpg ~ mtcars$disp, main="Mileage Vs Displacement", xlab="Displacement", ylab = "Mileage", col="blue")
plot(mtcars$mpg ~ mtcars$disp, main="Mileage Vs Gross horsepower", xlab="Gross horsepower", ylab = "Mileage",col="green")
plot(mtcars$mpg ~ mtcars$disp, main="Mileage Vs Weight", xlab="Weight", ylab = "Mileage", col="magenta")
title(main = "Figure 1", outer = TRUE)

mydata <- mtcars
mydata$cyl <- as.factor(mydata$cyl)
mydata$am <- as.factor(mydata$am)
mydata$gear <- as.factor(mydata$gear)

my_fit <- lm(mpg~., data = mydata)
step(my_fit)

fit2 <- lm(mpg~wt + qsec + am + factor(cyl), data = mydata)
fit3 <- lm(mpg~wt + qsec + am + factor(cyl) + disp, data = mydata)
fit4 <- lm(mpg~wt + am + factor(cyl) + disp, data = mydata)
anova(fit1, fit2, fit3, fit4)

fit2 <- lm(mpg~wt + am + factor(cyl), data = mydata)
fit3 <- lm(mpg~wt + am + factor(cyl) + disp, data = mydata)
anova(fit1, fit2, fit3)

fit1 <- lm(mpg~wt + qsec + am, data = mydata)
fit2 <- lm(mpg~wt + am , data = mydata)
fit3 <- lm(mpg~wt + am + factor(cyl), data = mydata)
fit4 <- lm(mpg~wt + am + factor(cyl) + disp, data = mydata)
anova(fit1, fit2, fit3, fit4)


