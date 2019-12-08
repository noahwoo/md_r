# mtcar exploratory analysis

library(knitr)
install.packages("printr")
library(printr)
kable(head(mtcars),align = 'c')

install.packages("GGally")
library(GGally)
library(ggplot2)

ggpairs(mtcars, 
        lower = list(continuous = "smooth",params = c(method = "loess", colour="blue")),
        diag=list(continuous="bar", params=c(colour="blue")),
        upper=list(params=list(corSize=15)), 
        axisLabels='show')

library(stats)
ggplot(mtcars, aes(y=mpg, x=factor(am, labels = c("automatic", "manual")), fill=factor(am)))+
  geom_violin(colour="black", size=1)+
  xlab("transmission") + ylab("MPG")

# difference between auto and manual significant?
test <- t.test(mpg ~ am, data = mtcars, var.equal = FALSE, 
               paired=FALSE, conf.level = .95)
result <- data.frame( "t-statistic"  = test$statistic, 
                      "df" = test$parameter,
                      "p-value"  = test$p.value,
                      "lower CL" = test$conf.int[1],
                      "upper CL" = test$conf.int[2],
                      "automatic mean" = test$estimate[1],
                      "manual mean" = test$estimate[2],
                      row.names = "")
kable(x = round(result,3),align = 'c')

# let us check the regression with one predictor `am'
mtcars$amfactor <- factor(mtcars$am, labels = c("automatic", "manual")) 
summary(lm(mpg ~ factor(amfactor), data = mtcars))$coef

# how about using more predictors? multi-collinearity appears and SE of regretors increase
summary(lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars))$coef
library(car)
fitvif <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
kable(vif(fitvif),align = 'c')

# select the siginificant ones with stepwise
library(MASS)
fit <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
step <- stepAIC(fit, direction="both", trace=TRUE)
summary(step)$coeff
# predictors selected: wt, qsec, factor(am)

# the r-square looks good
summary(step)$r.squared

# nested likelihood ratio test
fit1 <- lm(mpg ~ factor(am), data = mtcars)
fit2 <- lm(mpg ~ factor(am)+wt, data = mtcars)
fit3 <- lm(mpg ~ factor(am)+wt+qsec, data = mtcars)
fit4 <- lm(mpg ~ factor(am)+wt+qsec+hp, data = mtcars)
fit5 <- lm(mpg ~ factor(am)+wt+qsec+hp+drat, data = mtcars)
anova(fit1, fit2, fit3, fit4, fit5)

# now the predictors are finalized
finalfit <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)
summary(finalfit)$coef

# diagnostics of regression
library(grid)
library(gridExtra)
library(ggplot2)
library(lattice)

# vif again
fitvif <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)
kable(vif(fitvif),align = 'c')
# normality
qqPlot(finalfit, main="Normal Q-Q plot")
grid.arrange(diagPlts[["rvlevPlot"]],diagPlts[["cvlPlot"]],diagPlts[["cdPlot"]], ncol=3)
