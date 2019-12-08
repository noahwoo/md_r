# make up the data
age <- c(24, 18, 37, 45, 30, 28, 40, 55, 47, 26)
edu <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 1)
salary <- c(12, 2, 15, 10, 14, 7, 18, 6, 20, 19)
dat <- data.frame(age, edu, salary)
dat$edu <- factor(dat$edu)
summary(dat)

library("rpart")
library("rpart.plot")

train <- dat[1:7,]
test <- dat[8:10,]
fit <- rpart(salary ~ age + edu, data=train, method="anova", 
      control=rpart.control(minsplit=3, cp=0.001))
printcp(fit)
summary(fit)

# plot(fit, uniform=TRUE, 
#      main="Regression for Salary")
prp(fit, fallen.leaves = FALSE, 
    type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1)
# text(fit, use.n=FALSE, all=TRUE, cex=.8)
predict(fit, train)
predict(fit, test)