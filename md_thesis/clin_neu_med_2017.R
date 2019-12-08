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

hn_raw <- read.xlsx("/Users/wujianmin/Documents/personal/lp/0916-HN/HN0917_V5_1125.xlsx", 
                    sheetName = "HN2-retest2")
names(hn_raw)

# cleaning stuff
cols <- c("pathscore", "ADCav", "KtransAv", "SUVav", 
          "ADCmin", "SUVmax", "VeAv", "KepAv", "IAUGCAv")
cols2 <- c("pathscore", "ADCmean", "Ktrans", "SUVmean", 
          "ADCmin", "SUVmax", "Ve", "Kep", "IAUGCmean")
hn_c <- hn_raw[, cols] # project column

# factorize target variables
# hn_c$pathscore[hn_c$pathscore==1] <- 0
# hn_c$pathscore[hn_c$pathscore==2] <- 1
hn_c$pathscore <- as.factor(hn_c$pathscore)
hn_c <- hn_c[!is.na(hn_c$pathscore), ]

summary(hn_c$pathscore)
# clear problematic case
# hn_c <- hn_c[-13, ] # remove Wang Zhengyang
# hn_c <- hn_c[-19, ] # remove Xiang Xiaoyi
# hn_c <- hn_c[-23, ] # remove An Zhanzu
# hn_c <- hn_c[c(1:8, 10:12, 14, 16:18, 20:22, 24), ] # remove ALL
hn_c <- hn_c[c(1:12, 14:18, 20:22, 24), ] # remove ALL

chisq.test(c(0, 0), p = c(9/23, 14/23))
chisq.test(c(3, 1), p = c(9/23, 14/23))
chisq.test(c(2, 5), p = c(9/23, 14/23))
chisq.test(c(4, 8), p = c(9/23, 14/23))


# mark the current state of processing
hn <- hn_c
summary(hn)
# visual check data distribution
attach(hn)
par(mfrow=c(2,1))
plot(SUVav, ADCav, col=c("red", "green", "blue")[pathscore], main="SUVav vs. ADCav")
text(SUVav, ADCav, labels = row.names(hn), cex = 0.8)

plot(KtransAv, ADCav, col=c("red", "green", "blue")[pathscore], main="KtransAv vs. ADCav")
text(KtransAv, ADCav, labels = row.names(hn), cex = 0.8)
detach(hn)

# chisq test for t-stage and XSSC
M <- as.table(rbind(c(3,2,4), c(1,5,8)))
dimnames(M) <- list(ssc=c("W", "P"), ts=c("t2", "t3", "t4"))
M
chisq.test(M)

M <- as.table(rbind(c(6,4), c(9,14)))
chisq.test(M)

M <- as.table(rbind(c(1,4), c(9,14)))
chisq.test(M)

M <- as.table(rbind(c(1,2), c(9,14)))
chisq.test(M)

M <- as.table(rbind(c(0,3), c(9,14)))
chisq.test(M)

M <- as.table(rbind(c(1,0), c(9,14)))
chisq.test(M)

M <- as.table(rbind(c(0,1), c(9,14)))
chisq.test(M)

# purify the columns and rows
hn_p <- hn[, cols]
hn_p <- hn_p[!is.na(hn_p$pathscore), ]

# some stats here
hn_d <- hn_p[!is.na(hn_p$ADCav), ]
mean(hn_d[hn_d$pathscore==1, ]$ADCav)
mean(hn_d[hn_d$pathscore==2, ]$ADCav)

hn_d <- hn_p[!is.na(hn_p$SUVmax), ]
mean(hn_d[hn_d$pathscore==1, ]$SUVmax)
mean(hn_d[hn_d$pathscore==2, ]$SUVmax)

hn_d <- hn_p[!is.na(hn_p$SUVav), ]
mean(hn_d[hn_d$pathscore==1, ]$SUVav)
mean(hn_d[hn_d$pathscore==2, ]$SUVav)

hn_d <- hn_p[!is.na(hn_p$KtransAv), ]
mean(hn_d[hn_d$pathscore==1, ]$KtransAv)
mean(hn_d[hn_d$pathscore==2, ]$KtransAv)

hn_d <- hn_p[!is.na(hn_p$KepAv), ]
mean(hn_d[hn_d$pathscore==1, ]$KepAv)
mean(hn_d[hn_d$pathscore==2, ]$KepAv)

hn_d <- hn_p[!is.na(hn_p$VeAv), ]
mean(hn_d[hn_d$pathscore==1, ]$VeAv)
mean(hn_d[hn_d$pathscore==2, ]$VeAv)

hn_d <- hn_p[!is.na(hn_p$IAUGCAv), ]
mean(hn_d[hn_d$pathscore==1, ]$IAUGCAv)
mean(hn_d[hn_d$pathscore==2, ]$IAUGCAv)

# do anova test here
# hn_p <- hn_p[hn_p$pathscore != 2, ]
t.test(SUVav ~ pathscore, data=hn_p)
# fit <- aov(SUVav ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(SUVav ~ pathscore, data = hn_p)

t.test(SUVmax ~ pathscore, data=hn_p)
# fit <- aov(SUVmax ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(SUVmax ~ pathscore, data = hn_p)

t.test(ADCav ~ pathscore, data=hn_p)
# fit <- aov(ADCav ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(ADCav ~ pathscore, data = hn_p)

t.test(KtransAv ~ pathscore, data=hn_p)
# fit <- aov(KtransAv ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(KtransAv ~ pathscore, data = hn_p)
 
t.test(VeAv ~ pathscore, data=hn_p)
# fit <- aov(VeAv ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(VeAv ~ pathscore, data = hn_p)

t.test(KepAv ~ pathscore, data=hn_p)
# fit <- aov(KepAv ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(KepAv ~ pathscore, data = hn_p)

t.test(IAUGCAv ~ pathscore, data=hn_p)
# fit <- aov(IAUGCAv ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(IAUGCAv ~ pathscore, data = hn_p)

# box-plot with ggplot
library(reshape2)
library(ggplot2)
hn_c <- hn_p
names(hn_c) <- cols2
hn_c$pathscore <- c("WSCC", "M-PSCC")[hn_c$pathscore] 
hn.m <- melt(hn_c, id.vars='pathscore', 
             measure.vars=c('SUVmean','ADCmean','Ktrans',
                            'SUVmax','Ve','Kep'))
# ggplot(hn.m) +
#   geom_boxplot(aes(x=pathscore, y=value, color=variable))
p <- ggplot(data = hn.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=pathscore))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + scale_fill_manual(
  values = c("blue", "red"),
  name = "肿瘤分化程度",
  labels = c("WSCC", "M-PSCC")) 
p <- p + theme(text = element_text(family = 'SimSun'))
p

# remove some rows
# hn_p <- hn_p[!is.na(hn_p$ADCav), ]
# hn_p <- hn_p[!is.na(hn_p$KepAv), ]

summary(hn_p)

# correlation check
cor(hn_p[, cols[c(-1, -9, -5)]], method=c("pearson"))
cor(hn_p[, cols[c(-1, -9, -5)]], method=c("spearman"))

cor.test( ~ ADCav + SUVav, data = hn_p, method = c("spearm"))
cor.test( ~ ADCav + SUVmax, data = hn_p, method = c("spearm"))
cor.test( ~ ADCav + KtransAv, data = hn_p, method = c("spearm"))
cor.test( ~ ADCav + VeAv, data = hn_p, method = c("spearm"))
cor.test( ~ ADCav + KepAv, data = hn_p, method = c("spearm"))

cor.test( ~ SUVav + SUVmax, data = hn_p, method = c("spearm"))
cor.test( ~ SUVav + KtransAv, data = hn_p, method = c("spearm"))
cor.test( ~ SUVav + VeAv, data = hn_p, method = c("spearm"))
cor.test( ~ SUVav + KepAv, data = hn_p, method = c("spearm"))

cor.test( ~ SUVmax + KtransAv, data = hn_p, method = c("spearm"))
cor.test( ~ SUVmax + VeAv, data = hn_p, method = c("spearm"))
cor.test( ~ SUVmax + KepAv, data = hn_p, method = c("spearm"))

cor.test( ~ KtransAv + VeAv, data = hn_p, method = c("spearm"))
cor.test( ~ KtransAv + KepAv, data = hn_p, method = c("spearm"))

cor.test( ~ VeAv + KepAv, data = hn_p, method = c("spearm"))

# sig. test
t.test(SUVav ~ pathscore, data=hn_p)
wilcox.test(SUVav ~ pathscore, data=hn_p)

t.test(ADCmin ~ pathscore, data=hn_p)
wilcox.test(ADCmin ~ pathscore, data=hn_p)

t.test(ADCav ~ pathscore, data=hn_p)
wilcox.test(ADCav ~ pathscore, data=hn_p)

t.test(KtransAv ~ pathscore, data=hn_p)
wilcox.test(KtransAv ~ pathscore, data=hn_p)

t.test(VeAv ~ pathscore, data=hn_p)
wilcox.test(VeAv ~ pathscore, data=hn_p)

t.test(KepAv ~ pathscore, data=hn_p)
wilcox.test(KepAv ~ pathscore, data=hn_p)

t.test(IAUGCAv ~ pathscore, data=hn_p)
wilcox.test(IAUGCAv ~ pathscore, data=hn_p)

# evaluate on train
# cols <- c("pathscore", "ADCav", "KtransAv", "SUVav", 
#           "ADCmin", "SUVmax", "VeAv", "KepAv", "IAUGCAv")

hn_p <- hn_p[!is.na(hn_p$ADCav), ]
hn_p <- hn_p[!is.na(hn_p$KtransAv), ]
hn_p["9", "pathscore"] <- 1
hn_p$pathscore0 <- hn_p$pathscore==2


fit <- glm(pathscore0 ~ SUVmax + SUVav + ADCav + VeAv + KtransAv + KepAv, 
           family=binomial(link = "logit"), hn_p)
summary(fit)
step(fit)
pred <- predict(fit, hn_p)

fit0 <- glm(pathscore0 ~ SUVav + KepAv, 
           family=binomial(link = "logit"), hn_p)
summary(fit0)
pred0 <- predict(fit0, hn_p)

par(mfrow=c(1,1))
# SUVmean
plot(roc(hn_p$pathscore0, -hn_p$SUVav, direction="<", smooth = FALSE),
     col="blue",  main="", tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     xlab = "1-特异度(1-Specifity)", ylab = "敏感度(Sensitivity)",
     family='STKaiti', legacy.axes = TRUE)
suv <- roc_suite(hn_p$pathscore, -hn_p$SUVav)

# ADCmean
lines(roc(hn_p$pathscore, hn_p$ADCav, direction="<", smooth = FALSE),
      col="red", lwd=1)
adc <- roc_suite(hn_p$pathscore, hn_p$ADCav)

# SUVmax
lines(roc(hn_p$pathscore, -hn_p$SUVmax, direction="<", smooth = FALSE),
      col="green", lwd=1)
suvm <- roc_suite(hn_p$pathscore, -hn_p$SUVmax)

# VeAv
lines(roc(hn_p$pathscore, hn_p$VeAv, direction="<", smooth = FALSE),
      col="purple", lwd=1)
ve <- roc_suite(hn_p$pathscore, hn_p$VeAv)

# KepAv
lines(roc(hn_p$pathscore, -hn_p$KepAv, direction="<", smooth = FALSE),
      col="darkcyan", lwd=1)
kep <- roc_suite(hn_p$pathscore, -hn_p$KepAv)

# KtransAv
lines(roc(hn_p$pathscore, hn_p$KtransAv, direction="<", smooth = FALSE),
      col="indianred4", lwd=1)
trans <- roc_suite(hn_p$pathscore, hn_p$KtransAv)

# 组合
lines(roc(hn_p$pathscore, pred0, direction="<", smooth = FALSE),
      col="black", lwd=1)
comb <- roc_suite(hn_p$pathscore, pred0)

legend("bottomright", legend = c("SUVmean", "ADCmean", "SUVmax",
                                 "Ve", "Kep", "Ktrans", "组合"),
       lty = 1,  bty = "n", col=c("blue", "red", "green", "purple", 
                                  "darkcyan", "indianred4", "black"))


# evaluate with cross validation(LOO)
ntrain <- dim(hn_p)[1]
gpred1 <- (1:ntrain)
gpred2 <- (1:ntrain)
gpred3 <- (1:ntrain)
gpred4 <- (1:ntrain)
gpred5 <- (1:ntrain)
gpred6 <- (1:ntrain)
gpred7 <- (1:ntrain)

hn_p["17", "SUVav"] <- 6.09
for(k in 1:ntrain) {
  hn_trn <- hn_p[-k, ]
  hn_tst <- hn_p[k, ]
  fit1 <- glm(pathscore ~ SUVav, family=binomial(link = "logit"), hn_trn)
  fit2 <- glm(pathscore ~ ADCav, family=binomial(link = "logit"), hn_trn)
  fit3 <- glm(pathscore ~ KepAv, family=binomial(link = "logit"), hn_trn)
  fit4 <- glm(pathscore ~ KtransAv, family=binomial(link = "logit"), hn_trn)
  fit5 <- glm(pathscore ~ SUVmax, family=binomial(link = "logit"), hn_trn)
  fit6 <- glm(pathscore ~ VeAv, family=binomial(link = "logit"), hn_trn)
  fit7 <- glm(pathscore ~ KtransAv + SUVav + ADCav + KepAv, 
              family=binomial(link = "logit"), hn_trn)
  # fit5 <- rpart(pathscore ~ SUVav + ADCav + KepAv, data=hn_trn, method="anova", 
  #             control=rpart.control(minsplit=2, cp=0.001))
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
}

library(car)
fit5 <- glm(pathscore ~ SUVav + ADCav + KepAv, 
            family=binomial(link = "logit"), hn_p)
vif(fit5)
summary(fit5)

# table(gpred, hn_p$pathscore)
hn_p$pred <- gpred4

# we evaluate it here
require(ROCR)
library(pROC)
par(mfrow=c(1,1))

# roc plot
# perf <- performance(prediction(gpred, hn_p$pathscore), "tpr", "fpr")
# plot(perf, avg="threshold")
# suv
plot(roc(hn_p$pathscore, gpred1, direction="<", smooth = FALSE),
     col="blue",  main="", tck = 0.02, lwd=1,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     xlab = "1-特异度(1-Specifity)", ylab = "敏感度(Sensitivity)",
     family='STKaiti', legacy.axes = TRUE)
suv <- roc_suite(hn_p$pathscore, gpred1)

# adc
lines(roc(hn_p$pathscore, gpred2, direction="<", smooth = FALSE),
      col="red", lwd=1)
adc <- roc_suite(hn_p$pathscore, gpred2)

# kep
lines(roc(hn_p$pathscore, gpred3, direction="<", smooth = FALSE),
      col="green", lwd=1)
kep <- roc_suite(hn_p$pathscore, gpred3)

# ktrans
lines(roc(hn_p$pathscore, -gpred4, direction="<", smooth = FALSE),
      col="purple", lwd=1)
trans <- roc_suite(hn_p$pathscore, -gpred4)

# suvm
lines(roc(hn_p$pathscore, -gpred5, direction="<", smooth = FALSE),
      col="darkcyan", lwd=1)
suvm <- roc_suite(hn_p$pathscore, -gpred5)

# ve
lines(roc(hn_p$pathscore, -gpred6, direction="<", smooth = FALSE),
      col="indianred4", lwd=1)
ve <- roc_suite(hn_p$pathscore, -gpred6)

# combined
lines(roc(hn_p$pathscore, gpred7, direction="<", smooth = FALSE),
      col="black", lwd=1)
comb <- roc_suite(hn_p$pathscore, gpred7)

legend("bottomright", expression("SUVmean","ADCmean","Kep",
                                 "Ktrans", "SUVmax", "Ve", "Combined"), 
       lty = 1, col=c("blue", "red", "green", "purple", 
                      "darkcyan", "indianred4", "black"))

################################
sort(hn_p[hn_p$pathscore==1, ]$KepAv)
sort(hn_p[hn_p$pathscore==3, ]$KepAv)
# box-plot
# par(mfrow=c(2,2), family='STKaiti')
par(mfrow=c(1,2))
boxplot(SUVav ~ pathscore, data=hn_p, main = "SUVMean", names=c("Moderate-Poorly", "Well"))
boxplot(ADCav ~ pathscore, data=hn_p, main = "ADCMean",  names=c("Moderate-Poorly", "Well"))
par(mfrow=c(1,2))
boxplot(KepAv ~ pathscore, data=hn_p, main = "KepMean", names=c("Moderate-Poorly", "Well"))
boxplot(KtransAv ~ pathscore, data=hn_p, main = "KtransMean", names=c("Moderate-Poorly", "Well"))
par(mfrow=c(1,2))
boxplot(VeAv ~ pathscore, data=hn_p, main = "VeMean", names=c("Moderate-Poorly", "Well"))
boxplot(IAUGCAv ~ pathscore, data=hn_p, main = "IAUGCMean", names=c("Moderate-Poorly", "Well"))
