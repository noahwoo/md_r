require(xlsx)

hn_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN.xlsx", sheetName = "HN4")
# cleaning stuff
hn_c <- hn_raw[, c(4, 29:58)] # project column
hn_c$mgrp <- as.factor(hn_c$mgrp) # factorize
hn <- hn_c[!is.na(hn_c$ADC_focusM), ] # ignore other colums due to co-missing 
summary(hn)

# let us do some stat now
t.test(SUVmax3DPMC ~ mgrp, data=hn)
t.test(SUVav3DPMC ~ mgrp, data=hn)
t.test(ADC_focusM ~ mgrp, data=hn)
t.test(ADC_focusAv ~ mgrp, data=hn)

par(mfrow=c(2,2))
boxplot(SUVmax3DPMC ~ mgrp, data=hn, main = "SUVmax3DPMC")
boxplot(SUVav3DPMC ~ mgrp, data=hn, main = "SUVav3DPMC")
boxplot(ADC_focusM ~ mgrp, data=hn, main = "ADC_focusM")
boxplot(ADC_focusAv ~ mgrp, data=hn, main = "ADC_focusAv")

attach(hn)
par(mfrow=c(2,1))
plot(SUVav3DPMC, ADC_focusAv, col=c("red", "green")[mgrp], main="SUV vs. ADC(Avg)")
plot(SUVmax3DPMC, ADC_focusM, col=c("red", "green")[mgrp], main="SUV vs. ADC(Max)")
detach(hn)

# more temp validate
hn <- hn[!is.na(hn_c$MaxSlopeAv), ]

t.test(MaxSlopeAv ~ mgrp, data=hn)
t.test(CERAv ~ mgrp, data=hn)
t.test(IAUGCAv ~ mgrp, data=hn)
t.test(BATAv ~ mgrp, data=hn)
t.test(KtransAv ~ mgrp, data=hn)
t.test(VeAv ~ mgrp, data=hn)
t.test(KepAv ~ mgrp, data=hn)

attach(hn)
par(mfrow=c(2,1))
plot(SUVav3DPMC, MaxSlopeAv, col=c("red", "green")[mgrp], main="SUV vs. MS(Avg)")
plot(SUVav3DPMC, KtransAv, col=c("red", "green")[mgrp], main="SUV vs. IAUGC(Avg)")
detach(hn)

## data round2
ln_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN_V2.xlsx", sheetName = "LN")
names(ln_raw)
# cleaning stuff
ln_c <- ln_raw[, c(3, 8:35)] # project column
ln_c$LN <- as.factor(ln_c$LN) # factorize
ln <- ln_c[!is.na(ln_c$ADCmean), ] # ignore other colums due to co-missing 
summary(ln)

# t-test
t.test(SUVmean ~ LN, data=ln)
t.test(ADCmean ~ LN, data=ln)

# more temp validate
ln <- ln_c[!is.na(ln_c$MaxSlopeAv), ]

t.test(MaxSlopeAv ~ LN, data=ln)
t.test(CERAv ~ LN, data=ln)
t.test(IAUGCAv ~ LN, data=ln)
t.test(BATAv ~ LN, data=ln)
t.test(KtransAv ~ LN, data=ln)
t.test(VeAv ~ LN, data=ln)
t.test(KepAv ~ LN, data=ln)

t.test(MaxSlopeM ~ LN, data=ln)
t.test(CERM ~ LN, data=ln)
t.test(IAUGCM ~ LN, data=ln)
t.test(BATM ~ LN, data=ln)
t.test(KtransM ~ LN, data=ln)
t.test(VeM ~ LN, data=ln)
t.test(KepM ~ LN, data=ln)

attach(ln)
par(mfrow=c(2,1))
plot(SUVmean, KtransAv, col=c("red", "green")[LN], main="SUV vs. Ktrans(Avg)")
plot(SUVmean, KtransM, col=c("red", "green")[LN], main="SUV vs. Ktrans(Max)")
detach(ln)

## data round3
hn_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN_V3-2.xlsx", sheetName = "PL2")
names(hn_raw)
# cleaning stuff
hn_c <- hn_raw[, c(4, 7, 29:58)] # project column
hn_c$mgrp <- as.factor(hn_c$mgrp) # factorize
hn_c$tstage <- as.factor(hn_c$tstage) # factorize
hn <- hn_c[!is.na(hn_c$ADC_focusM), ] # ignore other colums due to co-missing 
# anova 
fit1 <- aov(SUVmax3DPMC ~ tstage, data=hn) # OK
summary(fit1)
fit2 <- aov(ADC_focusAv ~ tstage, data=hn) # OK
summary(fit2)
fit3 <- aov(KtransM ~ tstage, data=hn) # bad
summary(fit3)
fit4 <- aov(KtransAv ~ tstage, data=hn) # bad
summary(fit4)
fit4 <- aov(MaxSlopeAv ~ tstage, data=hn) # bad
summary(fit4)
fit4 <- aov(IAUGCAv ~ tstage, data=hn) # bad
summary(fit4)
fit4 <- aov(BATAv ~ tstage, data=hn) # bad
summary(fit4)
fit4 <- aov(VeAv ~ tstage, data=hn) # bad
summary(fit4)
fit4 <- aov(KepAv ~ tstage, data=hn) # bad
summary(fit4)

# hn <- hn[hn$mgrp != 2, ]
# hn <- hn[hn$mgrp != 1, ]
hn <- hn[hn$mgrp != 3, ]
summary(hn)

# let us do some stat now
t.test(SUVmax3DPMC ~ mgrp, data=hn)
t.test(SUVav3DPMC ~ mgrp, data=hn)
t.test(ADC_focusM ~ mgrp, data=hn)
t.test(ADC_focusAv ~ mgrp, data=hn)

## data round2
ln_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN_V2-2.xlsx", sheetName = "LN2")
names(ln_raw)
# cleaning stuff
ln_c <- ln_raw[, c(3, 8:35)] # project column
ln_c$LN <- as.factor(ln_c$LN) # factorize
ln <- ln_c[!is.na(ln_c$ADCmean), ] # ignore other colums due to co-missing 
summary(ln)

# t-test
t.test(SUVmean ~ LN, data=ln)
t.test(ADCmean ~ LN, data=ln)

ln <- ln_c[!is.na(ln_c$ASLrCBF), ]
t.test(ASLrCBF ~ LN, data = ln)
# more temp validate
ln <- ln_c[!is.na(ln_c$MaxSlopeAv), ]

t.test(MaxSlopeAv ~ LN, data=ln)
t.test(CERAv ~ LN, data=ln)
t.test(IAUGCAv ~ LN, data=ln)
t.test(BATAv ~ LN, data=ln)
t.test(KtransAv ~ LN, data=ln)
t.test(VeAv ~ LN, data=ln)
t.test(KepAv ~ LN, data=ln)

t.test(MaxSlopeM ~ LN, data=ln)
t.test(CERM ~ LN, data=ln)
t.test(IAUGCM ~ LN, data=ln)
t.test(BATM ~ LN, data=ln)
t.test(KtransM ~ LN, data=ln)
t.test(VeM ~ LN, data=ln)
t.test(KepM ~ LN, data=ln)

attach(ln)
par(mfrow=c(2,1))
plot(SUVmean, KtransAv, col=c("red", "green")[LN], main="SUV vs. Ktrans(Avg)")
plot(SUVmean, KtransM, col=c("red", "green")[LN], main="SUV vs. Ktrans(Max)")
detach(ln)

## data round4
hn_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN_0917.xlsx", sheetName = "HN4")
names(hn_raw)
# cleaning stuff
hn_c <- hn_raw[, c(4, 29:59)] # project column
hn_c$mgrp <- as.factor(hn_c$mgrp) # factorize

# anova ADC_min
hn <- hn_c[!is.na(hn_c$ADC_min), ] # ignore other colums due to co-missing 
fit1 <- aov(ADC_min ~ mgrp, data=hn) # OK
summary(fit1)

# anova ADC_Av
hn <- hn_c[!is.na(hn_c$ADC_Av), ] # ignore other colums due to co-missing 
fit2 <- aov(ADC_Av ~ mgrp, data=hn) # OK
summary(fit2)

# anova SUVmax
hn <- hn_c[!is.na(hn_c$SUVmax), ] # ignore other colums due to co-missing 
fit3 <- aov(SUVmax ~ mgrp, data=hn) # OK
summary(fit3)


# t-test 1 vs 3
hn <- hn_c[hn_c$mgrp != 2, ]
hn <- hn[!is.na(hn$ADC_Av), ]
t.test(ADC_Av ~ mgrp, data=hn)

hn <- hn_c[hn_c$mgrp != 2, ]
hn <- hn[!is.na(hn$ADC_min), ]
t.test(ADC_Av ~ mgrp, data=hn)

hn <- hn_c[hn_c$mgrp != 2, ]
hn <- hn[!is.na(hn$SUVmax), ]
t.test(SUVmax ~ mgrp, data=hn)

########################################################
## data round5
hn_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN0917_V5.xlsx", sheetName = "HN2-retest2")
names(hn_raw)
# cleaning stuff
hn_c <- hn_raw[, c(7, 34:37, 48, 52, 53, 54)] # project column
hn_c$pathscore <- as.factor(hn_c$pathscore) # factorize
# hn_c$pathscore[hn_c$pathscore==2] <- 3
names(hn_c)
summary(hn_c)

# anova SUVmax
hn <- hn_c[!is.na(hn_c$SUVmax), ] # ignore other colums due to co-missing 
fit <- aov(SUVmax ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# anova SUVav
hn <- hn_c[!is.na(hn_c$SUVav), ] # ignore other colums due to co-missing 
fit <- aov(SUVav ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# anova ADC_min (bad)
hn <- hn_c[!is.na(hn_c$ADCmin), ] # ignore other colums due to co-missing 
fit <- aov(ADCmin ~ pathscore, data=hn) # bad
summary(fit)
plot(TukeyHSD(fit))

# anova ADC_Av
hn <- hn_c[!is.na(hn_c$ADCav), ] # ignore other colums due to co-missing 
fit <- aov(ADCav ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# anova IAUGCAv (bad)
hn <- hn_c[!is.na(hn_c$IAUGCAv), ] # ignore other colums due to co-missing 
fit <- aov(IAUGCAv ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# anova KtransAv 
hn <- hn_c[!is.na(hn_c$KtransAv), ] # ignore other colums due to co-missing 
fit <- aov(KtransAv ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# anova VeAv (bad)
hn <- hn_c[!is.na(hn_c$VeAv), ] # ignore other colums due to co-missing 
fit <- aov(VeAv ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# anova KepAv (bad)
hn <- hn_c[!is.na(hn_c$KepAv), ] # ignore other colums due to co-missing 
fit <- aov(KepAv ~ pathscore, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))

# box-plot with ggplot
library(reshape2)
library(ggplot2)
hn.m <- melt(hn, id.vars='pathscore', 
              measure.vars=c('SUVmax','SUVav','ADCav','KtransAv','IAUGCAv','VeAv','KepAv'))
# ggplot(hn.m) +
#   geom_boxplot(aes(x=pathscore, y=value, color=variable))
p <- ggplot(data = hn.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=pathscore))
p <- p + facet_wrap( ~ variable, scales="free")
# p <- p + ggtitle("Boxplot of Pathological Score")
p

## data round6
hn_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN0917_V5.xlsx", 
                    sheetName = "HN2-retest_pred")
names(hn_raw)
# cleaning stuff
hn_c <- hn_raw[, c(7, 9:10, 12:14, 34:37, 48, 52:54)] # project column
hn_c$pathscore <- as.factor(hn_c$pathscore) # factorize
hn_c$N_same <- as.factor(hn_c$N_same) # factorize
hn_c$N_off <- as.factor(hn_c$N_off) # factorize
hn_c$Tstage <- as.factor(hn_c$Tstage)
hn_c$LN_path <- as.factor(hn_c$LN_path)
hn_c$LN_path2 <- as.factor(hn_c$LN_path2)

# refine label
hn_c$Tstage[hn_c$Tstage==2] <- 3

# sample filtering
# hn_c <- hn_c[!is.na(hn_c$Tstage), ]
hn_c <- hn_c[!is.na(hn_c$LN_path2), ]
hn_c <- hn_c[!is.na(hn_c$SUVav), ]
hn_c <- hn_c[!is.na(hn_c$ADCav), ]
# hn_c <- hn_c[!is.na(hn_c$IAUGCAv), ]
hn_c <- hn_c[!is.na(hn_c$KtransAv), ]
# hn_c <- hn_c[!is.na(hn_c$VeAv), ]
# hn_c <- hn_c[!is.na(hn_c$KepAv), ]

hn <- hn_c
names(hn)
summary(hn)

c(sd(hn$SUVmax), mean(hn$SUVmax))
c(sd(hn$SUVav), mean(hn$SUVav))
c(sd(hn$ADCav), mean(hn$ADCav))
c(sd(hn$KtransAv), mean(hn$KtransAv))
c(sd(hn$IAUGCAv), mean(hn$IAUGCAv))
c(sd(hn$VeAv), mean(hn$VeAv))
c(sd(hn$KepAv), mean(hn$KepAv))

# anova SUVmax
hn <- hn[!is.na(hn_c$SUVmax), ] # ignore other colums due to co-missing 
fit <- aov(SUVmax ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(SUVmax ~ Tstage, data=hn)
t.test(SUVmax ~ LN_path2, data=hn)

# anova SUVav
hn <- hn_c[!is.na(hn_c$SUVav), ] # ignore other colums due to co-missing 
fit <- aov(SUVav ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(SUVav ~ Tstage, data=hn)
t.test(SUVav ~ LN_path2, data=hn)

# anova ADC_min (bad)
hn <- hn_c[!is.na(hn_c$ADCmin), ] # ignore other colums due to co-missing 
fit <- aov(ADCmin ~ Tstage, data=hn) # bad
summary(fit)
plot(TukeyHSD(fit))
t.test(ADCmin ~ Tstage, data=hn)
t.test(ADCmin ~ LN_path2, data=hn)

# anova ADC_Av (bad)
hn <- hn_c[!is.na(hn_c$ADCav), ] # ignore other colums due to co-missing 
fit <- aov(ADCav ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(ADCav ~ Tstage, data=hn)
t.test(ADCav ~ LN_path2, data=hn)

# anova IAUGCAv (bad)
hn <- hn_c[!is.na(hn_c$IAUGCAv), ] # ignore other colums due to co-missing 
fit <- aov(IAUGCAv ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(IAUGCAv ~ Tstage, data=hn)
t.test(IAUGCAv ~ LN_path2, data=hn)

# anova KtransAv 
hn <- hn_c[!is.na(hn_c$KtransAv), ] # ignore other colums due to co-missing 
summary(hn)
fit <- aov(KtransAv ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(KtransAv ~ Tstage, data=hn)
t.test(KtransAv ~ LN_path2, data=hn)

# anova VeAv (bad)
hn <- hn_c[!is.na(hn_c$VeAv), ] # ignore other colums due to co-missing 
fit <- aov(VeAv ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(VeAv ~ Tstage, data=hn)
t.test(VeAv ~ LN_path2, data=hn)

# anova KepAv (bad)
hn <- hn_c[!is.na(hn_c$KepAv), ] # ignore other colums due to co-missing 
fit <- aov(KepAv ~ Tstage, data=hn) # OK
summary(fit)
plot(TukeyHSD(fit))
t.test(KepAv ~ Tstage, data=hn)
t.test(KepAv ~ LN_path2, data=hn)

# regression then
# fit <- glm(N_same ~ SUVmax + SUVav + ADCmin + ADCav + IAUGCAv + KtransAv + VeAv + KepAv, 
#            family=binomial(link = "logit"), hn)

fit1 <- glm(LN_path2 ~ SUVmax + VeAv + KepAv, 
           family=binomial(link = "logit"), hn)
summary(fit1)
pred <- predict(fit1, newdata = hn, type = c("response"))
print(pred)
hn$pred <- pred
t.test(pred ~ LN_path2, data=hn)

# show the result
require(ROCR)
auc <- performance(prediction(pred, hn$LN_path2),"auc")
auc
perf <- performance(prediction(pred, hn$LN_path2), "tpr", "fpr")
plot(perf)

# boxplot it
# box-plot with ggplot
library(reshape2)
library(ggplot2)
hn.m <- melt(hn, id.vars='LN_path2', 
             measure.vars=c('SUVmax','SUVav','ADCav','KtransAv','IAUGCAv','VeAv','KepAv'))
# ggplot(hn.m) +
#   geom_boxplot(aes(x=pathscore, y=value, color=variable))
p <- ggplot(data = hn.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=LN_path2))
p <- p + facet_wrap( ~ variable, scales="free")
# p <- p + ggtitle("Boxplot of Pathological Score")
p

## data round7

ln_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN0917_V5.xlsx", sheetName = "LN-retest2")
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

M <- cbind(ln$SUVmax, ln$SUVmean, ln$ADCmean, ln$IAUGCAv, ln$KtransAv, ln$VeAv, ln$KepAv)
cor(M, method=c("pearson"))
cor(M, method=c("kendall"))