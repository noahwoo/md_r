# require(xlsx)
require(readxl)
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


# clin-neu-med 
hn_raw <- read_excel("/Users/wujianmin/Documents/personal/lp/0916-HN/HN0917_V5.xlsx", 
                    sheet = "LN-retest2")
names(hn_raw)
hn_raw <- hn_raw[1:44, ]
# cleaning stuff
cols <- c("LN", "SUVmean", "SUVmax", "ADCmean", "IAUGCAv", "KtransAv", "VeAv", "KepAv")
hn_c <- hn_raw[, cols] # project column
hn_c <- hn_c[!is.na(hn_c$ADCmean), ]
hn_c <- hn_c[!is.na(hn_c$KtransAv), ]
summary(as.factor(hn_c$LN))
# clear problematic case
# hn_c <- hn_c[-13, ] # remove Wang Zhengyang
# hn_c <- hn_c[-19, ] # remove Xiang Xiaoyi
# hn_c <- hn_c[-23, ] # remove An Zhanzu
# hn_c <- hn_c[c(1:8, 10:12, 14, 16:18, 20:22, 24), ] # remove ALL
# hn_c <- hn_c[c(1:12, 14:18, 20:22, 24), ] # remove ALL

# some statistics
mean(hn_c$SUVmean[hn_c$LN==0])
mean(hn_c$SUVmean[hn_c$LN==1])
sd(hn_c$SUVmean[hn_c$LN==0])
sd(hn_c$SUVmean[hn_c$LN==1])

mean(hn_c$SUVmax[hn_c$LN==0])
mean(hn_c$SUVmax[hn_c$LN==1])
sd(hn_c$SUVmax[hn_c$LN==0])
sd(hn_c$SUVmax[hn_c$LN==1])


mean(hn_c$ADCmean[hn_c$LN==0])
mean(hn_c$ADCmean[hn_c$LN==1])
sd(hn_c$ADCmean[hn_c$LN==0])
sd(hn_c$ADCmean[hn_c$LN==1])

mean(hn_c$KtransAv[hn_c$LN==0])
mean(hn_c$KtransAv[hn_c$LN==1])
sd(hn_c$KtransAv[hn_c$LN==0])
sd(hn_c$KtransAv[hn_c$LN==1])

mean(hn_c$VeAv[hn_c$LN==0])
mean(hn_c$VeAv[hn_c$LN==1])
sd(hn_c$VeAv[hn_c$LN==0])
sd(hn_c$VeAv[hn_c$LN==1])

mean(hn_c$KepAv[hn_c$LN==0])
mean(hn_c$KepAv[hn_c$LN==1])
sd(hn_c$KepAv[hn_c$LN==0])
sd(hn_c$KepAv[hn_c$LN==1])

chisq.test(c(0, 0), p = c(7/16, 9/16))
chisq.test(c(4, 2), p = c(7/16, 9/16))
chisq.test(c(2, 3), p = c(7/16, 9/16))
chisq.test(c(1, 4), p = c(7/16, 9/16))


# coorelation
cor(hn_c[, cols[c(-1, -5)]], method=c("pearson"))
cor(hn_c[, cols[c(-1, -5)]], method=c("spearman"))

cor.test(hn_c[, "SUVmean"] , hn_c[, "SUVmax"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmean"] , hn_c[, "ADCmean"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmean"] , hn_c[, "KtransAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmean"] , hn_c[, "VeAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmean"] , hn_c[, "KepAv"],alternative = "two.sided", method="spearman")

cor.test(hn_c[, "SUVmax"] , hn_c[, "ADCmean"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmax"] , hn_c[, "KtransAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmax"] , hn_c[, "VeAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "SUVmax"] , hn_c[, "KepAv"],alternative = "two.sided", method="spearman")

cor.test(hn_c[, "ADCmean"] , hn_c[, "KtransAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "ADCmean"] , hn_c[, "VeAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "ADCmean"] , hn_c[, "KepAv"],alternative = "two.sided", method="spearman")

cor.test(hn_c[, "KtransAv"] , hn_c[, "VeAv"],alternative = "two.sided", method="spearman")
cor.test(hn_c[, "KtransAv"] , hn_c[, "KepAv"],alternative = "two.sided", method="spearman")

cor.test(hn_c[, "VeAv"] , hn_c[, "KepAv"],alternative = "two.sided", method="spearman")

# t-test and wilcox-test
t.test(SUVmean ~ LN, data=hn_c)
wilcox.test(SUVmean ~ LN, data=hn_c)

t.test(SUVmax ~ LN, data=hn_c)
wilcox.test(SUVmax ~ LN, data=hn_c)

t.test(ADCmean ~ LN, data=hn_c)
wilcox.test(ADCmean ~ LN, data=hn_c)

t.test(IAUGCAv ~ LN, data=hn_c)
wilcox.test(IAUGCAv ~ LN, data=hn_c)

t.test(KtransAv ~ LN, data=hn_c)
wilcox.test(KtransAv ~ LN, data=hn_c)

t.test(VeAv ~ LN, data=hn_c)
wilcox.test(VeAv ~ LN, data=hn_c)

t.test(KepAv ~ LN, data=hn_c)
wilcox.test(KepAv ~ LN, data=hn_c)

hn_c$LNS <- c("NPT", "PT")[hn_c$LN+1]
hn_c$Ktrans <- hn_c$KtransAv
hn_c$Ve <- hn_c$VeAv
hn_c$Kep <- hn_c$KepAv

hn.m <- melt(hn_c, id.vars='LNS', 
             measure.vars=c('SUVmean','SUVmax', 'ADCmean', 'Ktrans', 'Ve', 'Kep'))
# vars_alias <- c('SUVmean','SUVmax', 'ADCmean', 'Ktrans', 'Ve', 'Kep')
# ggplot(hn.m) +
#   geom_boxplot(aes(x=pathscore, y=value, color=variable))
p <- ggplot(data = hn.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=LNS))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + scale_fill_manual(
  values = c("blue", "red"),
  name = "淋巴结",
  labels = c("非转移", "转移"))
p <- p + theme(text = element_text(family = 'SimSun'))
p

# a full regression
fit <- glm(LN ~ SUVmean + ADCmean + VeAv + KtransAv + KepAv, 
            family=binomial(link = "logit"), hn_c)
vif(fit)
summary(fit)
step(fit)
pred <- predict(fit, hn_c)

fit0 <- glm(LN ~ SUVmax + ADCmean + VeAv, 
            family=binomial(link = "logit"), hn_c) # 0.7628
vif(fit0)
summary(fit0)

pred0 <- predict(fit0, hn_c)
summary(pred0)

# traditional version
par(mfrow=c(1,1), family = 'STKaiti')

# SUVmean
plot(roc(hn_c$LN, hn_c$SUVmean, direction="<", smooth = FALSE),
     col="blue", lwd=1, main="", tck = 0.02,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     xlab = "1-特异度(1-Specifity)", ylab = "敏感度(Sensitivity)",
     family='STKaiti', legacy.axes = TRUE)
suv <- roc_suite(hn_c$LN, hn_c$SUVmean)

# ADCmean
lines(roc(hn_c$LN, -hn_c$ADCmean, direction="<", smooth = FALSE),
      col="red", lwd=1)
adc <- roc_suite(hn_c$LN, -hn_c$ADCmean)

# SUVmax
lines(roc(hn_c$LN, hn_c$SUVmax, direction="<", smooth = FALSE),
      col="green", lwd=1)
suvm <- roc_suite(hn_c$LN, hn_c$SUVmax)

# VeAv
lines(roc(hn_c$LN, hn_c$VeAv, direction="<", smooth = FALSE),
      col="purple", lwd=1)
ve <- roc_suite(hn_c$LN, hn_c$VeAv)

# KepAv
lines(roc(hn_c$LN, hn_c$KepAv, direction="<", smooth = FALSE),
      col="darkcyan", lwd=1)
kep <- roc_suite(hn_c$LN, hn_c$KepAv)

# KtransAv
lines(roc(hn_c$LN, hn_c$KtransAv, direction="<", smooth = FALSE),
      col="indianred4", lwd=1)
trans <- roc_suite(hn_c$LN, hn_c$KtransAv)

# 组合
lines(roc(hn_c$LN, pred, direction="<", smooth = FALSE),
      col="black", lwd=1)
comb <- roc_suite(hn_c$LN, pred)

legend("bottomright", legend = c("SUVmean", "ADCmean", "SUVmax",
                                 "Ve", "Kep", "Ktrans", "组合"),
       lty = 1, bty = "n", col=c("blue", "red", "green", "purple", 
                      "darkcyan", "indianred4", "black"))


mcnemar.test(suv$ptar, suvm$ptar)
mcnemar.test(suv$ptar, adc$ptar)
mcnemar.test(suv$ptar, ve$ptar)
mcnemar.test(suv$ptar, kep$ptar)
mcnemar.test(suv$ptar, trans$ptar)

mcnemar.test(suvm$ptar, adc$ptar)
mcnemar.test(suvm$ptar, ve$ptar)
mcnemar.test(suvm$ptar, kep$ptar)
mcnemar.test(suvm$ptar, trans$ptar)

mcnemar.test(adc$ptar, ve$ptar)
mcnemar.test(adc$ptar, kep$ptar)
mcnemar.test(adc$ptar, trans$ptar)

mcnemar.test(ve$ptar, kep$ptar)
mcnemar.test(ve$ptar, trans$ptar)

mcnemar.test(kep$ptar, trans$ptar)

# game over, but we want something more...
# cross-validation version
ntrain <- dim(hn_c)[1]
gpred0 <- (1:ntrain)
gpred1 <- (1:ntrain)
gpred2 <- (1:ntrain)
gpred3 <- (1:ntrain)
gpred4 <- (1:ntrain)
gpred5 <- (1:ntrain)
gpred6 <- (1:ntrain)
gpred7 <- (1:ntrain)
gpred8 <- (1:ntrain)

library("rpart")
library("rpart.plot")

for(k in 1:ntrain) {
  hn_trn <- hn_c[-k, ]
  hn_tst <- hn_c[k, ]
  fit0 <- glm(LN ~ SUVmax + ADCmean + VeAv, 
              family=binomial(link = "logit"), hn_trn) # 0.7628
  fit1 <- glm(LN ~ SUVmean + ADCmean + VeAv + KtransAv + KepAv, 
             family=binomial(link = "logit"), hn_trn) # 0.6667
  fit2 <- glm(LN ~ SUVmean, 
              family=binomial(link = "logit"), hn_trn) # 0.7371
  fit3 <- glm(LN ~ ADCmean, 
              family=binomial(link = "logit"), hn_trn) # 0.6218
  fit4 <- glm(LN ~ SUVmax, 
              family=binomial(link = "logit"), hn_trn) # 0.6859
  fit5 <- glm(LN ~ VeAv, 
              family=binomial(link = "logit"), hn_trn) # 0.7371
  fit6 <- glm(LN ~ KepAv, 
              family=binomial(link = "logit"), hn_trn) # 0.7371
  fit7 <- glm(LN ~ KtransAv, 
              family=binomial(link = "logit"), hn_trn) # 0.7371
  # fit8 <- rpart(LN ~ SUVmean + ADCmean + KepAv + VeAv + IAUGCAv + KtransAv, 
  #               data=hn_trn, method="anova", 
  #               control=rpart.control(minsplit=8, cp=0.01))
  
  # prediction
  pred <- predict(fit0, newdata = hn_tst, type=c("response"))
  gpred0[k] <- pred[1]
  pred <- predict(fit1, newdata = hn_tst, type=c("response"))
  gpred1[k] <- pred[1]
  pred <- predict(fit2, newdata = hn_tst, type=c("response"))
  gpred2[k] <- pred[1]
  pred <- predict(fit3, newdata = hn_tst, type=c("response"))
  gpred3[k] <- pred[1]
  pred <- predict(fit4, newdata = hn_tst, type=c("response"))
  gpred4[k] <- pred[1]
  pred <- predict(fit5, newdata = hn_tst, type=c("response"))
  gpred5[k] <- pred[1]
  pred <- predict(fit6, newdata = hn_tst, type=c("response"))
  gpred6[k] <- pred[1]
  pred <- predict(fit7, newdata = hn_tst, type=c("response"))
  gpred7[k] <- pred[1]
  
  # pred <- predict(fit6, hn_tst)
  # gpred6[k] <- pred[1]
}

par(mfrow=c(1,1))

# SUVmean
plot(roc(hn_c$LN, gpred2, direction="<", smooth = FALSE),
     col="blue", lwd=1, main="MR、PET、ADC及组合的ROC", 
     xlab = "特异度", ylab = "敏感度", 
     family='STKaiti')
suv <- roc_suite(hn_c$LN, gpred2)

# ADCmean
lines(roc(hn_c$LN, gpred3, direction="<", smooth = FALSE),
      col="red", lwd=1)
adc <- roc_suite(hn_c$LN, gpred3)

# SUVmax
lines(roc(hn_c$LN, gpred4, direction="<", smooth = FALSE),
      col="green", lwd=1)
suvm <- roc_suite(hn_c$LN, gpred4)

# VeAv
lines(roc(hn_c$LN, gpred5, direction="<", smooth = FALSE),
      col="purple", lwd=1)
ve <- roc_suite(hn_c$LN, gpred5)

# KepAv
lines(roc(hn_c$LN, -gpred6, direction="<", smooth = FALSE),
      col="darkcyan", lwd=1)
kep <- roc_suite(hn_c$LN, -gpred6)

# TransAv
lines(roc(hn_c$LN, gpred7, direction="<", smooth = FALSE),
      col="indianred4", lwd=1)
trans <- roc_suite(hn_c$LN, gpred7)

# 组合
lines(roc(hn_c$LN, gpred0, direction="<", smooth = FALSE),
      col="black", lwd=1)
comb <- roc_suite(hn_c$LN, gpred0)

legend("bottomright", legend = c("SUVmean", "ADCmean", "SUVmax",
                                 "VeAv", "KepAv", "TransAv", "组合"),
       lty = 1, col=c("blue", "red", "green", "purple", 
                      "darkcyan", "indianred4", "black"))


#################
s1 <- c(1,1,1,1,0,0,0,0,0,0,0)
s2 <- c(1,0,1,1,1,1)
s3 <- c(1,1,1,1,0,0,0,0,0,1,1,1,1)
s4 <- c(0,1,0,0,0,1,0,0,0,0,1,1,0)

t.test(s1, y = s2, paired=FALSE) # -2.08, 0.058
t.test(s1, y = s3, paired=FALSE) # -1.21, 0.237
t.test(s1, y = s4, paired=FALSE) # 0.276, 0.785
t.test(s2, y = s3, paired=FALSE) # 1, 0.337
t.test(s2, y = s4, paired=FALSE) # 2.46, 0.031
t.test(s3, y = s4, paired=FALSE) # 1.58, 0.125
