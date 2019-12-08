require(xlsx)
require(ROCR)
library(pROC)
library(car)
library(psych)

hn_chen <- read.xlsx("/Users/wujianmin/Documents/personal/lp/180112-HN/HN20180112.xlsx", 
                    sheetName = "LN_Dr_Chen")
names(hn_chen)
hn_chen$LN <- as.factor(hn_chen$LN)

hn_dang <- read.xlsx("/Users/wujianmin/Documents/personal/lp/180112-HN/HN20180112.xlsx", 
                     sheetName = "LN_Dr_Dang")
names(hn_dang)
hn_dang$LN <- as.factor(hn_dang$LN)

# weighted-kappa test
kp <- cohen.kappa(cbind(hn_chen$MR, hn_dang$MR))
kp$plevel
kp <- cohen.kappa(cbind(hn_chen$PET, hn_dang$PET))
kp$plevel
kp <- cohen.kappa(cbind(hn_chen$ADC, hn_dang$ADC))
kp$plevel

# select one labeler
hn_p <- hn_chen
hn_p$LN <- as.factor(hn_p$LN)
summary(hn_p)

# correlation
cor.test( ~ MR + PET, data = hn_p, method = c("spearm"))
cor.test( ~ PET + ADC, data = hn_p, method = c("spearm"))
cor.test( ~ ADC + MR, data = hn_p, method = c("spearm"))

# do some prediction
# auc & t.test
auc <- performance(prediction(hn_p$MR, hn_p$LN),"auc")
auc
t.test(MR ~ LN, data=hn_p)

auc <- performance(prediction(hn_p$PET, hn_p$LN),"auc")
auc
t.test(PET ~ LN, data=hn_p)

auc <- performance(prediction(hn_p$ADC, hn_p$LN),"auc")
auc
t.test(ADC ~ LN, data=hn_p)

auc <- performance(prediction(hn_p$PET.MR, hn_p$LN),"auc")
auc
t.test(PET.MR ~ LN, data=hn_p)

# LR
fit <- glm(LN ~ MR+PET+ADC, family=binomial(link = "logit"), hn_p)
summary(fit)
vif(fit)

pred <- predict(fit, newdata = hn_p, type=c("response"))
auc <- performance(prediction(pred, hn_p$LN),"auc")
auc

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

par(mfrow=c(1,1))
plot(roc(hn_p$LN, hn_p$MR, direction="<", smooth = FALSE),
     col="blue", lwd=1, main="MR、PET、ADC及组合的ROC", xlab = "特异度",
     ylab = "敏感度", 
     family='STKaiti')
mr <- roc_suite(hn_p$LN, hn_p$MR)

lines(roc(hn_p$LN, hn_p$PET, direction="<", smooth = FALSE),
      col="red", lwd=1)
pet <- roc_suite(hn_p$LN, hn_p$PET)

lines(roc(hn_p$LN, hn_p$ADC, direction="<", smooth = FALSE),
      col="green", lwd=1)
adc <- roc_suite(hn_p$LN, hn_p$ADC)

lines(roc(hn_p$LN, pred, direction="<", smooth = FALSE),
      col="purple", lwd=1)
comb <- roc_suite(hn_p$LN, pred)

legend("bottomright", legend = c("MR", "PET", "ADC","组合"),
       lty = 1, col=c("blue", "red", "green", "purple"))

# mcnemar test for consistency
mcnemar.test(mr$ptar, pet$ptar)
mcnemar.test(mr$ptar, adc$ptar)
mcnemar.test(mr$ptar, comb$ptar)
mcnemar.test(pet$ptar, adc$ptar)
mcnemar.test(pet$ptar, comb$ptar)
mcnemar.test(adc$ptar, comb$ptar)

attach(hn_p)
par(mfrow=c(3,1))
plot(MR, PET, col=c("red", "blue")[LN], main="MR vs. PET")
# text(MR, PET, labels = row.names(hn_p), cex = 0.8)

plot(PET, ADC, col=c("red", "blue")[LN], main="PET vs. ADC")
# text(PET, ADC, labels = row.names(hn_p), cex = 0.8)

plot(ADC, MR, col=c("red", "blue")[LN], main="ADC vs. MR")
# text(ADC, MR, labels = row.names(hn_p), cex = 0.8)
detach(hn_p)


age1 <- c(57
  ,53
  ,67
  ,57
  ,61
  ,71)

age2 <- c(68
          ,58
          ,52
          ,64
          ,61
          ,78
          ,53)

t.test(age1, age2)
summary(age1)

t1 <- c(
  2
  ,2
  ,3
  ,4
  ,2
  ,2
)

t2 <- c(
  4
  ,4
  ,4
  ,3
  ,3
  ,4
  ,4
)

t.test(t1, t2)
wilcox.test(t1, t2)

# chi-sq test
n1 <- c(4, 1, 1)
n2 <- c(0, 2, 5)
chisq.test(n1, n2)

ln  <- c(1,1,0,0,1,0,1,1,1,0,1,0,0)
age <- c(53,68,57,53,64,71,58,61,52,67,78,57,61)

ks.test((age-mean(age))/sd(age), mean(age)*rnorm(length(age)))
qqplot(age-mean(age)/sd(age),  mean(age)*rnorm(length(age)))

df <- data.frame(ln, age)
t.test(age ~ ln, data = df)
wilcox.test(age ~ ln, data = df)
