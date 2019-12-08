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


# clin-neu-med 
hn_raw <- read.xlsx("/Users/wujianmin/Documents/personal/lp/thesis/HN20180112-new.xlsx", 
                    sheetName = "HN2-retest_pred")
names(hn_raw)
hn_raw <- hn_raw[1:29, ]
cols <- c("pathscore",  "CellMo", "CellJiang", "SUVav", 
          "SUVmax", "ADCmin", "ADCav", "IAUGCAv", "KtransAv", "VeAv", "KepAv")
cols2 <- c("pathscore",  "CellMo", "CellJiang", "SUVmean", 
           "SUVmax", "ADCmin", "ADCmean", "IAUGC", "Ktrans", "Ve", "Kep")
cols3 <- c("pathscore", "CellMo", "CellJiang", "SUVmean", 
           "SUVmax", "ADCmin", "ADCmean", "IAUGCmean", "KtransMean", "VeMean", "KepMean")
hn_c <- hn_raw[, cols] # project column

# cleaning new labels
hn_c <- hn_c[!is.na(hn_c$SUVav), ]
hn_c <- hn_c[!is.na(hn_c$ADCav), ]
hn_c <- hn_c[!is.na(hn_c$CellMo), ]
# hn_c <- hn_c[!is.na(hn_cm$KtransMean), ] # optional

hn_cm <- hn_c
names(hn_cm) <- cols3
hn_cm["2", "CellMo"] <- 2 
hn_cm["21", "CellMo"] <- 2
hn_cm["8", "CellMo"] <- 2
hn_cm["10", "CellMo"] <- 2 
# hn_cm["20", "CellMo"] <- 3
hn_cm["19", "CellMo"] <- 2 

hn_cm$CellMo_f[hn_cm$CellMo <= 2.5] <- 0
hn_cm$CellMo_f[hn_cm$CellMo > 2.5] <- 1

hn_cm$CellMo_f <- as.factor(hn_cm$CellMo_f)
summary(hn_cm$CellMo_f)

hn_cm$pathscore_f <- (hn_cm$pathscore==3)
hn_cm$mix <- sprintf("%s_%s", hn_cm$CellMo_f, hn_cm$pathscore_f)
summary(as.factor(hn_cm$mix))

# visual of data
# box-plot
hn_cm$CellMo_t <- c("Low", "High")[hn_cm$CellMo_f]
hn.m <- melt(hn_cm, id.vars='CellMo_t', 
             measure.vars=c('SUVmean','ADCmean', 'SUVmax',
                            "Ktrans", "Ve", "Kep"))
# ggplot(hn.m) +
#   geom_boxplot(aes(x=pathscore, y=value, color=variable))
p <- ggplot(data = hn.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=CellMo_t))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + scale_fill_manual(
  values = c("blue", "red"),
  name = "EGFR",
  labels = c("高表达", "低表达")) 
p <- p + theme(text = element_text(family = 'SimSun'))

# p <- p + ggtitle("Boxplot of Pathological Score")
p

attach(hn_cm)
par(mfrow=c(1,2))
plot(SUVmean, KtransMean, col=c("red", "green", "blue", "purple", "yellow")[pathscore], main="SUVav/KtransAv(PathScore)")
text(SUVmean, KtransMean, labels = row.names(hn_c), cex = 0.8)

plot(SUVmean, KtransMean, col=c("red", "green", "blue", "purple", "yellow")[CellMo_f], main="SUVav/KtransAv(CellMo)")
text(SUVmean, KtransMean, labels = row.names(hn_c), cex = 0.8)
detach(hn_cm)

# shapiro.test(hn_cm$SUVmean)
# ad.test(hn_cm$SUVmean)
# qqnorm(hn_cm$SUVmean)
ks.test(hn_cm$SUVmean, rnorm(18))
ks.test(hn_cm$ADCmean, rnorm(18))
ks.test(hn_cm$SUVmax, rnorm(18))
ks.test(hn_cm$KtransMean, rnorm(18))
ks.test(hn_cm$KepMean, rnorm(18))
ks.test(hn_cm$VeMean, rnorm(18))

# correlation
cor.test(hn_cm$CellMo, hn_cm$SUVmean)
# t.test(SUVmean ~ CellMo_f, data=hn_cm)
wilcox.test(SUVmean ~ CellMo_f, data=hn_cm)

cor.test(hn_cm$CellMo, hn_cm$SUVmax)
# t.test(SUVmax ~ CellMo_f, data=hn_cm)
wilcox.test(SUVmax ~ CellMo_f, data=hn_cm)

cor.test(hn_cm$CellMo, hn_cm$ADCmean)
# t.test(ADCmean ~ CellMo_f, data=hn_cm)
wilcox.test(ADCmean ~ CellMo_f, data=hn_cm)

cor.test(hn_cm$CellMo, hn_cm$KtransMean)
# t.test(KtransMean ~ CellMo_f, data=hn_cm)
wilcox.test(KtransMean ~ CellMo_f, data=hn_cm)

cor.test(hn_cm$CellMo, hn_cm$KepMean)
# t.test(KepMean ~ CellMo_f, data=hn_cm)
wilcox.test(KepMean ~ CellMo_f, data=hn_cm)

cor.test(hn_cm$CellMo, hn_cm$VeMean)
# t.test(VeMean ~ CellMo_f, data=hn_cm)
wilcox.test(VeMean ~ CellMo_f, data=hn_cm)

# clear non-ktrans case
# hn_cm <- hn_cm[!is.na(hn_cm$KtransMean), ]

# a full regression
fit <- glm(CellMo_f ~ SUVmean + ADCmean + VeMean + KtransMean + KepMean, 
           family=binomial(link = "logit"), hn_cm)
vif(fit)
summary(fit)
step(fit)
pred <- predict(fit, hn_cm)

fit0 <- glm(CellMo_f ~ SUVmean + KtransMean + KepMean + VeMean, 
            family=binomial(link = "logit"), hn_cm) # 0.7628
vif(fit0)
summary(fit0)

pred0 <- predict(fit0, hn_cm)

# traditional version
par(mfrow=c(1,1), family = 'STKaiti')

# SUVmean
plot(roc(hn_cm$CellMo_f, hn_cm$SUVmean, direction="<", smooth = FALSE),
     col="blue",  main="", tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     xlab = "1-特异度(1-Specifity)", ylab = "敏感度(Sensitivity)",
     family='STKaiti', legacy.axes = TRUE)
suv <- roc_suite(hn_cm$CellMo_f, hn_cm$SUVmean)

# ADCmean
lines(roc(hn_cm$CellMo_f, hn_cm$ADCmean, direction="<", smooth = FALSE),
      col="red", lwd=1)
adc <- roc_suite(hn_cm$CellMo_f, hn_cm$ADCmean)

# SUVmax
lines(roc(hn_cm$CellMo_f, hn_cm$SUVmax, direction="<", smooth = FALSE),
      col="green", lwd=1)
suvm <- roc_suite(hn_cm$CellMo_f, hn_cm$SUVmax)

# VeAv
lines(roc(hn_cm$CellMo_f, -hn_cm$VeMean, direction="<", smooth = FALSE),
      col="purple", lwd=1)
ve <- roc_suite(hn_cm$CellMo_f, -hn_cm$VeMean)

# KepAv
lines(roc(hn_cm$CellMo_f, hn_cm$KepMean, direction="<", smooth = FALSE),
      col="darkcyan", lwd=1)
kep <- roc_suite(hn_cm$CellMo_f, hn_cm$KepMean)

# KtransAv
lines(roc(hn_cm$CellMo_f, -hn_cm$KtransMean, direction="<", smooth = FALSE),
      col="indianred4", lwd=1)
trans <- roc_suite(hn_cm$CellMo_f, -hn_cm$KtransMean)

# 组合
lines(roc(hn_cm$CellMo_f, pred0, direction="<", smooth = FALSE),
      col="black", lwd=1)
comb <- roc_suite(hn_cm$CellMo_f, pred0)

legend("bottomright", legend = c("SUVmean", "ADCmean", "SUVmax",
                                 "Ve", "Kep", "Trans", "组合"),
       lty = 1,  bty = "n", col=c("blue", "red", "green", "purple", 
                      "darkcyan", "indianred4", "black"))

mcnemar.test(comb$ptar, suv$ptar)
mcnemar.test(comb$ptar, suvm$ptar)
mcnemar.test(comb$ptar, adc$ptar)
mcnemar.test(comb$ptar, ve$ptar)
mcnemar.test(comb$ptar, kep$ptar)
mcnemar.test(comb$ptar, trans$ptar)

# table-1 confidence
chisq.test(c(3, 0), p = c(12/21, 9/21))
chisq.test(c(2, 5), p = c(12/21, 9/21))
chisq.test(c(7, 4), p = c(12/21, 9/21))

chisq.test(c(5, 2), p = c(12/21, 9/21))
chisq.test(c(7, 7), p = c(12/21, 9/21))

chisq.test(c(6, 6), p = c(12/21, 9/21))
chisq.test(c(6, 3), p = c(12/21, 9/21))

age1 <- c(
61,
58,
53,
50,
61,
82,
57,
54,
64)

age0 <- c(
51,
78,
71,
64,
68,
53,
53,
47,
66,
74,
56,
67)

t.test(age0, age1)
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
