require(xlsx)

## prediction of Lymph Node
ln_raw <- read.xlsx("/Users/wujianmin/Documents/personal/lp/0916-HN/HN0917_V5.xlsx", 
                    sheetName = "LN-retest2")
names(ln_raw)
# cleaning stuff
ln_c <- ln_raw[, c(2, 7:14)] # project column
ln_c$LN <- as.factor(ln_c$LN) # factorize
summary(ln_c)

# t-test
ln <- ln_c
t.test(SUVmax ~ LN, data=ln)
t.test(SUVmean ~ LN, data=ln)

# t-test
ln <- ln_c[!is.na(ln_c$ADCmean), ] # ignore other colums due to co-missing 
t.test(ADCmin ~ LN, data=ln)
t.test(ADCmean ~ LN, data=ln)

ln <- ln_c[!is.na(ln_c$Ktrans), ] # ignore other colums due to co-missing
ln <- ln[!is.na(ln$ADCmean), ]
summary(ln)
t.test(IAUGCAv ~ LN, data=ln)
t.test(KtransAv ~ LN, data=ln)
t.test(VeAv ~ LN, data=ln)
t.test(KepAv ~ LN, data=ln)

M <- cbind(ln$SUVmax, ln$SUVmean, ln$ADCmean, ln$IAUGCAv, 
           ln$KtransAv, ln$VeAv, ln$KepAv)
cor(M, method=c("pearson"))
cor(M, method=c("kendall"))

# search for optimal parameters
fit1 <- glm(LN ~ SUVmax + ADCmean 
            + IAUGCAv + KtransAv + VeAv + KepAv, 
            family=binomial(link = "logit"), ln)
summary(fit1)
library(car)
vif(fit1)
plot(ln[, 2:9])
sfit1 <- step(fit1)
summary(fit1)

# cross validation with LOO
ntrain <- dim(ln)[1]
gpred <- (1:ntrain)
for(k in 1:ntrain) {
  ln_trn <- ln[-k, ]
  ln_tst <- ln[k, ]
  # fit1 <- glm(LN ~ SUVmax + ADCmean +  KtransAv, 
  fit1 <- glm(LN ~ SUVmax, 
             family=binomial(link = "logit"), ln_trn)
  pred <- predict(fit1, newdata = ln_tst, type=c("response"))
  # fit2 <- svm(pathscore ~ SUVav+KtransAv, data = hn_p, 
  #             kernel="radial", cost=10)
  # pred <- predict(fit2, hn_tst)
  gpred[k] <- pred[1]
}

require(ROCR)
performance(prediction(gpred, ln$LN),"auc")
perf <- performance(prediction(gpred, ln$LN), "tpr", "fpr")
plot(perf)

# train with full data
fit1 <- glm(LN ~ SUVmax + ADCmean +  KtransAv, 
            family=binomial(link = "logit"), ln)
summary(fit1)

# single feature performance
par(mfrow=c(1,1))
gauc <- rep(0.0, 7)
names(gauc) <- c("SUVmax", "SUVmean", "ADCmean", "IAUGCAv", 
               "KtransAv", "VeAv", "KepAv")
plot.new()
for(fea in names(gauc)) {
  pred <- ln[, fea]
  if (fea == "ADCmean") {
    pred <- -pred
  }
  auc  <- performance(prediction(pred, ln$LN), "auc")
  gauc[fea] <- auc@y.values[[1]]
  perf <- performance(prediction(pred, ln$LN), "tpr", "fpr")
  plot(perf, add = TRUE, colorize = FALSE)
}

print(gauc)

3*6*30/2
