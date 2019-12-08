require(xlsx)
require(ROCR)
library(pROC)
library(car)
library(psych)

# boxplot with ggplot2
library(reshape2)
library(ggplot2)
library(nortest)

roc_suite <- function(target, pred) {
  roc   <- prediction(pred, target)
  auc   <- performance(roc, "auc")@y.values[[1]]
  perf  <- performance(roc, "sens", "spec")
  perfv <- perf@x.values[[1]] + perf@y.values[[1]]
  ix    <- which.max(perfv)
  cutoff <- perf@alpha.values[[1]][ix]
  sens  <- perf@y.values[[1]][ix] 
  spec  <- perf@x.values[[1]][ix]
  
  nppv  <- performance(roc, "ppv", "npv")
  npv   <- nppv@x.values[[1]][ix]
  ppv   <- nppv@y.values[[1]][ix]
  
  r0 <- roc(target, rep(0, length(target)))
  r1 <- roc(target, pred, direction="<", smooth = FALSE)
  test <- roc.test(r0, r1) # significant test
  ci   <- ci.auc(r1, method = c("bootstrap"))
  list(auc=auc, ix=ix, sens=sens, spec=spec, 
       npv=npv, ppv=ppv, youden=sens+spec-1, 
       cutoff=cutoff, pvalue=test$p.value,
       ci_lb=ci[1][[1]], ci_ub=ci[3][[1]], 
       ptar=as.factor(pred>cutoff))
}

is_raw <- read.xlsx("/Users/wujianmin/Documents/personal/lp/is-20181007/20181012.xls", 
                    sheetName = "Stat1")
names(is_raw)
is_cln <- is_raw[1:137, c("GenderE", "MonAge", "Therapy", "EEG", "Cause", "MR", "PET")]
is_cln$Cause <- as.factor(is_cln$Cause)
is_cln$GenderE <- as.factor(is_cln$GenderE)
is_cln$Therapy <- as.factor(is_cln$Therapy)
is_cln$EEG <- as.factor(is_cln$EEG)
is_cln$MR <- as.factor(is_cln$MR)
is_cln$PET <- as.factor(is_cln$PET)
is_cln$MonAge <- as.integer(is_cln$MonAge)

is_cln$MixTC <- 2*as.integer(is_cln$Therapy) + as.integer(is_cln$Cause)-3
is_cln$MixTC <- as.factor(is_cln$MixTC)

is_cln$MixTE <- 2*as.integer(is_cln$Therapy) + as.integer(is_cln$EEG)-3
is_cln$MixTE <- as.factor(is_cln$MixTE)

summary(is_cln)

# t-test for Cause to MonAge
t.test(MonAge ~ Cause, data = is_cln)
mean(is_cln$MonAge[is_cln$Cause==0])
sd(is_cln$MonAge[is_cln$Cause==0])
mean(is_cln$MonAge[is_cln$Cause==1])
sd(is_cln$MonAge[is_cln$Cause==1])

# chi-sq test of Cause to GenderE
summary(is_cln$GenderE[is_cln$Cause==0])
summary(is_cln$GenderE[is_cln$Cause==1])

chisq.test(is_cln$Cause, y = is_cln$GenderE)

# chi-sq test of Cause to EEG
summary(is_cln$EEG[is_cln$Cause==0])
summary(is_cln$EEG[is_cln$Cause==1])

chisq.test(is_cln$Cause, y = is_cln$EEG)

### use therapy as golden set
# t-test for therapy to MonAge
t.test(MonAge ~ Therapy, data = is_cln)
mean(is_cln$MonAge[is_cln$Therapy==0])
sd(is_cln$MonAge[is_cln$Therapy==0])
mean(is_cln$MonAge[is_cln$Therapy==1])
sd(is_cln$MonAge[is_cln$Therapy==1])

# chi-sq test of therapy to GenderE
summary(is_cln$GenderE[is_cln$Therapy==0])
summary(is_cln$GenderE[is_cln$Therapy==1])

chisq.test(is_cln$Therapy, y = is_cln$GenderE)

# chi-sq test of therapy to EEG
summary(is_cln$EEG[is_cln$Therapy==0])
summary(is_cln$EEG[is_cln$Therapy==1])

chisq.test(is_cln$Therapy, y = is_cln$EEG)

# chi-sq test of therapy to Cause
summary(is_cln$Cause[is_cln$Therapy==0])
summary(is_cln$Cause[is_cln$Therapy==1])

chisq.test(is_cln$Therapy, y = is_cln$Cause)

### chi-sq test of therapy to MR
chisq.test(is_cln$Therapy, y = is_cln$MR)
chisq.test(is_cln$Cause, y = is_cln$MR)

# is_cln$GenderEB <- 1
# is_cln$GenderEB[is_cln$GenderE=='F'] <- 0
# is_cln$GenderEB <- as.factor(is_cln$GenderEB)

### LR of therapy to Cause, EEG, MonAge, GenderE
# is_cln$MonAge2 <- is_cln$MonAge/max(is_cln$MonAge)

fit <- glm(Therapy ~ Cause + EEG + MonAge + GenderE, 
           family=binomial(link = "logit"), is_cln)
fit0 <- glm(Therapy ~ 1, 
           family=binomial(link = "logit"), is_cln)

R2 <- 1-as.vector(logLik(fit)/logLik(fit0))

pred <- predict(fit, newdata = is_cln, type=c("response"))

fit.step <- step(fit)

summary(fit.step)$r.square

fit.step$coeff #提取首列-回归系数
fit.step$coefficients#默认提取首列回归系数
lmResults<-summary(fit.step)#将逐步回归结果赋值给a
lmResults
lmResults$r.squared#提取R方
lmResults$adj.r.squared#提取调整R方Adjusted R-squared
lmResults$fstatistic#F


# require(rms)
# printed(fit)
