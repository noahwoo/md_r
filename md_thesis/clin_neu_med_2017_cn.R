# require(xlsx)
require(readxl)

hn_raw <- read_excel("/Users/wujianmin/Documents/personal/lp/0916-HN/HN0917_V5_1125.xlsx", 
                    sheet = "HN2-retest2")
names(hn_raw)

hn_raw$pathscore_t[hn_raw$pathscore_t==2] <- 1
t.test(age ~ pathscore_t, hn_raw)
t.test(T_stage ~ pathscore_t, hn_raw)

# cleaning stuff
cols <- c("pathscore", "ADCav", "KtransAv", "SUVav", 
          "ADCmin", "SUVmax", "VeAv", "KepAv", "IAUGCAv")
cols2 <- c("pathscore", "ADCmean", "Ktrans_mean", "SUVmean", 
          "ADCmin", "SUVmax", "Ve_mean", "Kep_mean", "IAUGCmean")
hn_c <- hn_raw[, cols] # project column

# "SUVmean", "SUVmax", "ADCmean"
library(nortest)
shapiro.test(hn_c$SUVav)
ad.test(hn_c$SUVav)
qqnorm(hn_c$SUVav)
ks.test(hn_c$SUVav, rnorm(23))

shapiro.test(hn_c$SUVmax)
ad.test(hn_c$SUVmax)
qqnorm(hn_c$SUVmax)
ks.test(hn_c$SUVmax, rnorm(23))

shapiro.test(hn_c$ADCav)
ad.test(hn_c$ADCav)
qqnorm(hn_c$ADCav)
ks.test(hn_c$ADCav, rnorm(23))

# factorize target variables
hn_c$pathscore <- as.factor(hn_c$pathscore)

# clear problematic case
# hn_c <- hn_c[-13, ] # remove Wang Zhengyang
# hn_c <- hn_c[-19, ] # remove Xiang Xiaoyi
# hn_c <- hn_c[-23, ] # remove An Zhanzu
# hn_c <- hn_c[c(1:8, 10:12, 14, 16:18, 20:22, 24), ] # remove ALL
hn_c <- hn_c[c(1:12, 14:18, 20:22, 24), ] # remove ALL


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
wilcox.test(SUVav ~ pathscore, data=hn_p, paired = FALSE)

t.test(SUVmax ~ pathscore, data=hn_p)
# fit <- aov(SUVmax ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(SUVmax ~ pathscore, data = hn_p)
wilcox.test(SUVmax ~ pathscore, data=hn_p)

t.test(ADCav ~ pathscore, data=hn_p)
# fit <- aov(ADCav ~ pathscore, data=hn_p) # OK
# summary(fit)
# kruskal.test(ADCav ~ pathscore, data = hn_p)
wilcox.test(ADCav ~ pathscore, data=hn_p)



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

# box-plot

# box-plot with ggplot
library(reshape2)
library(ggplot2)
hn_c <- hn_p
names(hn_c) <- cols2
# hn_c$pathscore <- c("高分化", "中低分化")[hn_c$pathscore] 
hn_c$pathscore <- c("WSCC", "M-PSCC")[hn_c$pathscore]
hn.m <- melt(hn_c, id.vars='pathscore', 
             measure.vars=c('SUVmean','ADCmean', 'SUVmax'))
# ggplot(hn.m) +
#   geom_boxplot(aes(x=pathscore, y=value, color=variable))
p <- ggplot(data = hn.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=pathscore))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + scale_fill_manual(
  values = c("blue", "red"),
  name = "分化程度 Differentiation",
  labels = c("高分化 Well differentiation", "中低分化 Moderately-poorly differentiation")) 
p <- p + theme(text = element_text(family = 'SimSun'))

# p <- p + ggtitle("Boxplot of Pathological Score")
p
# remove some rows
hn_p <- hn_p[!is.na(hn_p$ADCav), ]
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
# auc
auc <- performance(prediction(-hn_p$KepAv, hn_p$pathscore),"auc")
auc
auc <- performance(prediction(-hn_p$SUVav, hn_p$pathscore),"auc")
auc
auc <- performance(prediction(hn_p$ADCav, hn_p$pathscore),"auc")
auc

# LR
fit1 <- glm(pathscore ~ SUVav, family=binomial(link = "logit"), hn_p)
pred1 <- predict(fit1, newdata = hn_p, type=c("response"))

require(ROCR)
library(pROC)
par(mfrow=c(1,1))

plot(roc(hn_p$pathscore, pred1, direction="<", smooth = FALSE),
     col="blue", lwd=2, main="", tck = 0.02,
     xaxs = "i", yaxs="i", 
     xlim = c(1, 0), ylim = c(0,1), 
     xlab = "1-特异度(1-Specifity)", ylab = "敏感度(Sensitivity)",
     family='STKaiti', legacy.axes = TRUE)

auc  <- performance(prediction(pred1, hn_p$pathscore), "auc")
auc@y.values[[1]]

fit2 <- glm(pathscore ~ ADCav, family=binomial(link = "logit"), hn_p)
pred2 <- predict(fit2, newdata = hn_p, type=c("response"))

lines(roc(hn_p$pathscore, pred2, direction="<", smooth = FALSE),
     col="red", lwd=2)
auc  <- performance(prediction(pred2, hn_p$pathscore), "auc")
auc@y.values[[1]]

fit3 <- glm(pathscore ~ ADCav + SUVav, family=binomial(link = "logit"), hn_p)
pred3 <- predict(fit3, newdata = hn_p, type=c("response"))

lines(roc(hn_p$pathscore, pred3, direction="<", smooth = FALSE),
      col="purple", lwd=2)
auc  <- performance(prediction(pred3, hn_p$pathscore), "auc")
auc@y.values[[1]]

legend("bottomright", expression("SUVmean", "ADCmean","Combination"), 
       lty = 1, bty = "n", col=c("blue", "red", "purple"))

# evaluate with cross validation(LOO)
hn_tp <- hn_p[!is.na(hn_p$KepAv), ]

mean(hn_p[hn_p$pathscore==1, "ADCav"])
mean(hn_p[hn_p$pathscore==1, "SUVmax"])
mean(hn_p[hn_p$pathscore==1, "SUVav"])

sd(hn_p[hn_p$pathscore==1, "ADCav"])
sd(hn_p[hn_p$pathscore==1, "SUVmax"])
sd(hn_p[hn_p$pathscore==1, "SUVav"])

mean(hn_p[hn_p$pathscore==2, "ADCav"])
mean(hn_p[hn_p$pathscore==2, "SUVmax"])
mean(hn_p[hn_p$pathscore==2, "SUVav"])

sd(hn_p[hn_p$pathscore==2, "ADCav"])
sd(hn_p[hn_p$pathscore==2, "SUVmax"])
sd(hn_p[hn_p$pathscore==2, "SUVav"])


hn_fi <- hn_p[is.na(hn_p$KepAv), ]
hn_fi[8, "ADCav"] <- 1.3 

attach(hn_fi)
par(mfrow=c(1,1))
plot(SUVav, ADCav, col=c("red", "green", "blue")[pathscore], main="SUVav vs. ADCav")
text(SUVav, ADCav, labels = row.names(hn_fi), cex = 0.8)
detach(hn_fi)

hn_fp <- rbind(hn_tp, hn_fi[c(1,2,3,4,5,6,8,9), ])

ntrain_t <- dim(hn_tp)[1]
ntrain_f <- dim(hn_fp)[1]

gpred1 <- (1:ntrain_t)
gpred2 <- (1:ntrain_t)
gpred3 <- (1:ntrain_f)
gpred4 <- (1:ntrain_f)
gpred5 <- (1:ntrain_f)

library("rpart")
library("rpart.plot")
hn_u <- hn_fp
ntrain <- dim(hn_u)[1]

for(k in 1:ntrain) {
  hn_trn <- hn_u[-k, ]
  hn_tst <- hn_u[k, ]
  fit1 <- glm(pathscore ~ SUVav, family=binomial(link = "logit"), hn_trn)
  fit2 <- glm(pathscore ~ ADCav, family=binomial(link = "logit"), hn_trn)
  fit3 <- glm(pathscore ~ SUVmax, family=binomial(link = "logit"), hn_trn)
  fit4 <- glm(pathscore ~ SUVav + ADCav, family=binomial(link = "logit"), hn_trn)
  fit5 <- glm(pathscore ~ SUVav + ADCav + SUVmax, family=binomial(link = "logit"), hn_trn)
  # fit5 <- rpart(pathscore ~ SUVav + ADCav + KepAv, data=hn_trn, method="anova", 
  #             control=rpart.control(minsplit=2, cp=0.001))
  if (ntrain == ntrain_t) {
    pred <- predict(fit1, newdata = hn_tst, type=c("response"))
    gpred1[k] <- pred[1]
    pred <- predict(fit2, newdata = hn_tst, type=c("response"))
    gpred2[k] <- pred[1]
    pred <- predict(fit3, newdata = hn_tst, type=c("response"))
    gpred3[k] <- pred[1]
  }

  if (ntrain == ntrain_f) {
    pred <- predict(fit4, newdata = hn_tst, type=c("response"))
    gpred4[k] <- pred[1]
    # pred <- predict(fit5, hn_tst)
    # pred <- predict(fit5, newdata = hn_tst, type=c("response"))
    # gpred5[k] <- pred[1]
  }
}

fit_ff <- glm(pathscore ~ SUVav + ADCav, family=binomial(link = "logit"), hn_fp)
summary(fit_ff)

library(car)
# we evaluate it here
require(ROCR)
library(pROC)
par(mfrow=c(1,1))

# roc plot
# perf <- performance(prediction(gpred, hn_p$pathscore), "tpr", "fpr")
# plot(perf, avg="threshold")
# suv
plot(roc(hn_tp$pathscore, gpred1, direction="<", smooth = FALSE),
     col="blue", lwd=2, main="SUV、ADC及组合的ROC", family='STKaiti')
auc  <- performance(prediction(gpred1, hn_tp$pathscore), "auc")
auc@y.values[[1]]
# sens/spec
perf <- performance(prediction(gpred1, hn_tp$pathscore), "sens", "spec")
perf
ix <- which.max(perf@x.values[[1]] + perf@y.values[[1]])
sens <- perf@y.values[[1]][ix] 
spec <- perf@x.values[[1]][ix]
c(ix=ix, sens=sens, spec=spec)
auc <- performance(prediction(gpred1, hn_tp$pathscore), "ppv", "npv")
auc

# adc
lines(roc(hn_tp$pathscore, gpred2, direction="<", smooth = FALSE),
     col="black", lwd=2)
auc  <- performance(prediction(gpred2, hn_tp$pathscore), "auc")
auc@y.values[[1]]
# sens/spec
perf <- performance(prediction(gpred2, hn_tp$pathscore), "sens", "spec")
perf
ix <- which.max(perf@x.values[[1]] + perf@y.values[[1]])
sens <- perf@y.values[[1]][ix] 
spec <- perf@x.values[[1]][ix]
c(ix=ix, sens=sens, spec=spec)
auc <- performance(prediction(gpred2, hn_tp$pathscore), "ppv", "npv")
auc

# suvmax
lines(roc(hn_tp$pathscore, gpred3, direction="<", smooth = FALSE),
      col="black", lwd=2)
auc  <- performance(prediction(gpred3, hn_tp$pathscore), "auc")
auc@y.values[[1]]


# combined
lines(roc(hn_fp$pathscore, gpred4, direction="<", smooth = FALSE),
      col="red", lwd=2)
auc  <- performance(prediction(gpred4, hn_fp$pathscore), "auc")
auc@y.values[[1]]

legend("bottomright", expression("SUVmean", "SUVmax", "ADCmean","Combination"), 
       lty = 1, col=c("blue", "red", "black", "purple"))

perf <- performance(prediction(gpred4, hn_fp$pathscore), "sens", "spec")
perfv <- perf@x.values[[1]] + perf@y.values[[1]]
ix <- which.max(perfv)
sens <- perf@y.values[[1]][ix] 
spec <- perf@x.values[[1]][ix]
c(ix=ix, sens=sens, spec=spec, cutoff=perfv[ix])

auc <- performance(prediction(gpred4, hn_fp$pathscore), "ppv", "npv")
auc
c(npv=auc@x.values[[1]][ix], ppv=auc@y.values[[1]][ix])


r0 <- roc(hn_fp$pathscore, rep(0, ntrain))
r1 <- roc(hn_fp$pathscore, gpred4, direction="<", smooth = FALSE)
roc.test(r0, r1) # significant test
ci.auc(r1, method = c("bootstrap"))

##############################
r0 <- roc(hn_p$pathscore, rep(0, ntrain)) 
# combine
r1 <- roc(hn_p$pathscore, gpred5, direction="<", smooth = FALSE)
roc.test(r0, r1) # significant test
ci.auc(r1, method = c("bootstrap"))

# suv_av
r2 <- roc(hn_p$pathscore, gpred1, direction="<", smooth = FALSE)
roc.test(r0, r2) # significant test
ci.auc(r2)

# adc_av
r3 <- roc(hn_p$pathscore, gpred2, direction="<", smooth = FALSE)
roc.test(r0, r3) # significant test
ci.auc(r3)

# suv_max
r4 <- roc(hn_p$pathscore, gpred3, direction=">", smooth = FALSE)
roc.test(r0, r4) # significant test
ci.auc(r4)

# ---------
roc.test(r1, r2) # suv_av
roc.test(r1, r3) # adc_av
roc.test(r1, r4) # suv_max
roc.test(r2, r3)

# sens/spec
perf <- performance(prediction(gpred3, hn_p$pathscore), "sens", "spec")
perf
ix <- which.max(perf@x.values[[1]] + perf@y.values[[1]])
sens <- perf@y.values[[1]][ix] 
spec <- perf@x.values[[1]][ix]
c(ix=ix, sens=sens, spec=spec)
auc <- performance(prediction(gpred3, hn_p$pathscore), "ppv", "npv")
auc


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

