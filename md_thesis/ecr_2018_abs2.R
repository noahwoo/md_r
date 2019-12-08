########################
## data round9 (predict here with LR)
########################
require(xlsx)
hn_raw <- read.xlsx("/Users/wujianmin/Documents/lp/0916-HN/HN0917_V5.xlsx", sheetName = "HN2-retest_LR")
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

hn <- hn_c
hn_p <- hn[, c("N_off", "ADCav", "KtransAv", "SUVav")]
hn_p <- hn_p[!is.na(hn_p$ADCav), ]
hn_p <- hn_p[!is.na(hn_p$KtransAv), ]

# again with LR
require(ROCR)
hn_p$N_off <- droplevels(hn_p$N_off)
fit2 <- glm(N_off ~ KtransAv + ADCav + SUVav, family=binomial(link = "logit"), hn_p)
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