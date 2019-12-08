########################
## data round8 (predict here with LR)
########################
require(xlsx)
hn_raw <- read.xlsx("/Users/wujianmin/Documents/personal/lp/0916-HN/HN0917_V5.xlsx", 
                    sheetName = "HN2-retest_LR")
names(hn_raw)

# cleaning stuff
hn_c <- hn_raw[, c(7, 9:10, 12:14, 34:37, 48, 52:54)] # project column
hc_c <- hn_c[1:23, ]
# factorize target variables
hn_c$pathscore <- as.factor(hn_c$pathscore)
hn_c$N_same <- as.factor(hn_c$N_same)
hn_c$N_off <- as.factor(hn_c$N_off)
hn_c$Tstage <- as.factor(hn_c$Tstage)
hn_c$LN_path <- as.factor(hn_c$LN_path)
hn_c$LN_path2 <- as.factor(hn_c$LN_path2)

# clean rows with NA columns
hn <- hn_c
# hn <- hn[!is.na(hn$ADCmin), ]
# hn <- hn[!is.na(hn$KtransAv), ]
# hn$pathscore[hn$pathscore==2] <- 1

# try anova here
fit <- aov(SUVav ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

fit <- aov(SUVmax ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

fit <- aov(ADCmin ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
fit <- aov(ADCav ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

fit <- aov(KtransAv ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# refine label
hn$pathscore[hn$pathscore==2] <- 1
# hn$pathscore[hn$pathscore==3] <- 2
hn$pathscore <- droplevels(hn$pathscore)
summary(hn)
row.names(hn)
attach(hn)
par(mfrow=c(1,1))
text(SUVav, SUVmax, labels = row.names(hn), cex = 0.8)
plot(SUVav, SUVmax, col=c("red", "green", "blue")[pathscore], main="SUV AV vs. MAX")

# plot(SUVmax, KtransAv, col=c("red", "green", "yellow")[pathscore], main="SUV vs. Ktrans")

par(mfrow=c(1,1))
plot(ADCmin, ADCav, col=c("red", "green", "blue")[pathscore], main="ADC MIN vs. AV")
text(ADCmin, ADCav, labels = row.names(hn), cex = 0.8)
# plot(ADCav, KtransAv, col=c("red", "green", "yellow")[pathscore], main="SUV vs. Ktrans")

par(mfrow=c(1,1))
plot(KtransAv, ADCav, col=c("red", "green", "blue")[pathscore], main="ADC MIN vs. AV")
text(KtransAv, ADCav, labels = row.names(hn), cex = 0.8)

detach(hn)

t.test(SUVmax ~ pathscore, data=hn)
t.test(SUVav ~ pathscore, data=hn)
t.test(ADCmin ~ pathscore, data=hn)
t.test(ADCav ~ pathscore, data=hn)
t.test(KtransAv ~ pathscore, data=hn)
t.test(VeAv ~ pathscore, data=hn)
t.test(KepAv ~ pathscore, data=hn)
t.test(IAUGCAv ~ pathscore, data=hn)

# stepwise feature selection
fit1 <- glm(pathscore ~ ADCav + KtransAv, 
            family=binomial(link = "logit"), hn)
summary(fit1)
# sfit1 <- step(fit1, scope = ~ ADCav 
#              + VeAv + KepAv + KtransAv + IAUGCAv, direction = "both",
#              trace = 1)
# summary(sfit1)
pred <- predict(fit1, newdata = hn, type = c("response"))
print(pred)
hn_c$pred <- pred
t.test(pred ~ pathscore, data=hn)

##########################
# predict with purer data 
hn_p <- hn[, c("pathscore", "ADCav", "KtransAv", "SUVav", 
               "ADCmin", "VeAv", "KepAv", "IAUGCAv")]
hn_p <- hn_p[!is.na(hn_p$ADCav), ]
hn_p <- hn_p[!is.na(hn_p$KtransAv), ]

# hn_p <- hn_p[-5, ] # Wang Zhengyang
# hn_p <- hn_p[-10, ] # Xiang Xiaoyi
hn_p <- hn_p[c(1:4, 6:9, 11:13), ] # All
ntrain <- dim(hn_p)[1]

attach(hn_p)
par(mfrow=c(2,1))
plot(SUVav, ADCav, col=c("red", "green", "blue")[pathscore], main="SUVav vs. ADCav")
text(SUVav, ADCav, labels = row.names(hn_p), cex = 0.8)

plot(KepAv, ADCav, col=c("red", "green", "blue")[pathscore], main="KepAv vs. ADCav")
text(KepAv, ADCav, labels = row.names(hn_p), cex = 0.8)
detach(hn_p)

gpred <- (1:ntrain)
for(k in 1:ntrain) {
  hn_trn <- hn_p[-k, ]
  hn_tst <- hn_p[k, ]
  # fit1 <- glm(pathscore ~ IAUGCAv, 
  fit1 <- glm(pathscore ~ SUVav + ADCav + KepAv, 
  # fit1 <- glm(pathscore ~ ADCav, 
             family=binomial(link = "logit"), hn_trn)
  pred <- predict(fit1, newdata = hn_tst, type=c("response"))
  # fit2 <- svm(pathscore ~ ADCmin + SUVav, data = hn_trn, 
  #             kernel="radial", cost=10)
  # pred <- predict(fit2, hn_tst)
  gpred[k] <- pred[1]
}
# table(gpred, hn_p$pathscore)
hn_p$pred <- gpred

# we evaluate it here
require(ROCR)
# auc
auc <- performance(prediction(gpred, hn_p$pathscore),"auc")
auc
# roc plot
perf <- performance(prediction(gpred, hn_p$pathscore), "tpr", "fpr")
plot(perf, avg="threshold")
# sens/spec
perf <- performance(prediction(gpred, hn_p$pathscore), "sens", "spec")
str(perf)
ix <- which.max(perf@x.values[[1]] + perf@y.values[[1]])
sens <- perf@y.values[[1]][ix] 
spec <- perf@x.values[[1]][ix]

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

#################################
# how about SVM?
library(e1071)
fit2 <- svm(pathscore ~ ADCav, data = hn_p, 
            kernel="radial", cost=50)
summary(fit2)
pred <- predict(fit2, hn_p)
print(pred)
print(hn_p$pathscore)
table(pred, hn_p$pathscore)
plot(fit2, hn_p, ADCav ~ KtransAv)

# again with LR
require(ROCR)
hn_p$pathscore <- droplevels(hn_p$pathscore)
fit2 <- glm(pathscore ~ KtransAv + ADCav + SUVav, family=binomial(link = "logit"), hn_p)
summary(fit2)
pred <- predict(fit2, hn_p)
performance(prediction(pred, hn_p$pathscore),"auc")
perf <- performance(prediction(pred, hn_p$pathscore), "tpr", "fpr")
plot(perf)

attach(hn_p)
par(mfrow=c(1,1))
text(SUVav, ADCav, labels = row.names(hn_p), cex = 0.8)
plot(SUVav, ADCav, col=c("red", "green", "blue")[pathscore], main="SUV vs. ADC")

par(mfrow=c(1,1))
plot(SUVav, KtransAv, col=c("red", "green", "blue")[pathscore], main="SUV vs. Ktrans")
text(SUVav, KtransAv, labels = row.names(hn_p), cex = 0.8)

par(mfrow=c(1,1))
plot(KtransAv, ADCav, col=c("red", "green", "blue")[pathscore], main="Ktrans vs. ADC")
text(KtransAv, ADCav, labels = row.names(hn_p), cex = 0.8)
detach(hn_p)

pred[pred >= 0] <- 3
pred[pred < 0] <- 1
print(pred)
print(hn_p$pathscore)
table(pred, hn_p$pathscore)

# Cross Validation by LOO
ntrain <- dim(hn)[1]
gpred <- (1:ntrain)
for(k in 1:ntrain) {
  hn_trn <- hn[-k, ]
  hn_tst <- hn[k, ]
  fit1 <- glm(pathscore ~ ADCav + KtransAv, 
              family=binomial(link = "logit"), hn_trn)
  pred <- predict(fit1, newdata = hn_tst, type=c("response"))
  gpred[k] <- pred[1]
}
t.test(gpred ~ pathscore, data=hn)

# show the result
require(ROCR)
auc <- performance(prediction(gpred, hn$pathscore),"auc")
auc
perf <- performance(prediction(pred, hn$pathscore), "tpr", "fpr")
plot(perf)